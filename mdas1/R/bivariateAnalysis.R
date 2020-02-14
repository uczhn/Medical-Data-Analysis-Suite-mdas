#' A bivariateAnalysis Function
#'
#' This function performs appropriate bivariate analysis based on the data type of dependent (DV) and independent (IV) variables. Pearson
#' correlation is used for numerical data, and Kendall Tau if data is ordinal. Chi-square test of association is used for nominal data, and when nominal data
#' are less than 5 in any category, the Fisher test is used instead. To analyse association between numerical and nominal data,
#' we first determine if the data distribution is normal using Shapiro-Wilk test. For normal distributed data, we use t-test for
#' two levels nominal data, and one-way ANOVA for 3-levels or more nominal data. We use non-parametric tests i.e. Mann-Whitney
#' for nominal data with 2 levels, and Kruskal-Wallis when there are more than 2 levels nominal data.
#'
#' @param dat data in data.frame format
#' @param na.rm TRUE if missing data is NOT to be used in analysis
#' @param plot TRUE if you wish to produce bivariate plots
#' @param warning TRUE to show all warnings generated from statistical analysis
#' @author Saisakul Chernbumroong, Henry  Nanji
#' @references \url{http://www.biostathandbook.com/testchoice.html}
#' @return Return statistics from bivariate analysis.
#' @keywords bivariate analysis
#' @export
#' @examples
#' data(iris)
#' result = bivariateAnalysis(iris)
bivariateAnalysis = function(dat, na.rm = T, plot = F, warning = F)
{

  if(requireNamespace("ggplot2", quietly = TRUE))
  results = data.frame()
  no.data = 0

  error <-NULL

  for(i in 1:ncol(dat))
  {
    for(j in 1:ncol(dat))
    {
      tryCatch(
        {

          df = dat[, c(i,j)]

          #Remove NA data
          if(na.rm)
          {
            df = stats::na.omit(df)
            no.data = nrow(df)

            if(nrow(df) == 0)
            {
              error("There is not enough data.", call. = F)
            }
          }

          x = df[, 1]
          y = df[, 2]

          xname = make.names(colnames(dat)[i])
          yname = make.names(colnames(dat)[j])

          #change ordinal to numeric data
          flag = F
          if(is.ordered(x))
          {
            x = as.integer(x)
            flag = T
          }

          if(is.ordered(y))
          {
            y = as.integer(y)
            flag = T
          }

          #For numerical VS numerical data
          if(is.numeric(x) & is.numeric(y))
          {
            if(flag)
            {
              cor = stats::cor.test(x, y, method = "kendall")
              test = "Kendall"
            }
            else
            {
              cor = stats::cor.test(x, y, method = "pearson")
              test = "Pearson"
            }

            # cor = cor.test(x, y, method = "spearman")
            # test = "Spearman"
            temp = cbind.data.frame("X" = xname,
                                    "Y" = yname,
                                    "Type" = test,
                                    "Statistic" = cor$estimate,
                                    "pval" = cor$p.value,
                                    "no.data" = no.data)
            results = rbind.data.frame(results, temp)

            #scatter plot
            if(plot)
            {
              pl = data.frame(x,y)
              colnames(pl) = c(xname, yname)
              p1 = ggplot2::ggplot(pl, ggplot2::aes_string(x = xname, y = yname)) + ggplot2::geom_point() +
                ggplot2::xlab(xname) + ggplot2::ylab(yname)
              print(p1)
            }
          }
          else if(!is.numeric(x) & !is.numeric(y)) #categorical VS categorical
          {

            tab = table(x,y)
            if(sum(tab <= 5))
            {
              cor = stats::fisher.test(x, y, simulate.p.value = TRUE, B = 1e5)
              test = "Fisher Exact"
              cor$statistic = NA
            }
            else
            {
              cor = stats::chisq.test(x, y)
              test = "Chi-square"
            }

            temp = cbind.data.frame("X" = xname,
                                    "Y" = yname,
                                    "Type" = test,
                                    "Statistic" = cor$statistic,
                                    "pval" = cor$p.value,
                                    "no.data" = no.data)
            results = rbind.data.frame(results, temp)

            #bar plot
            if(plot)
            {
              pl = data.frame(x,y)
              colnames(pl) = c(xname, yname)
              p1 = ggplot2::ggplot(pl, ggplot2::aes_string(x = xname,
                                         fill = yname)) + ggplot2::geom_bar(position="dodge")
              print(p1)

            }
          }
          else if((!is.numeric(x) & is.numeric(y)) | (is.numeric(x) & !is.numeric(y)))
          {
            #which one is numeric
            if(!is.numeric(x))
            {
              #y is numeric
              met = y
              non.met = x
              col.x = make.names(xname)
              col.y = make.names(yname)
            }
            else
            {
              met = x
              non.met = y
              col.x = make.names(yname)
              col.y = make.names(xname)
            }
            #prepare data
            level = levels(non.met)
            no.level = length(level)
            #check normality
            norm.test = stats::shapiro.test(met)
            if(norm.test$p.value >= 0.05)
            {
              #normal distribution, parametric test
              if(no.level > 2)
              {
                #one-way ANOVA

                temp = data.frame(as.numeric(non.met), met)
                colnames(temp) = c("y", "x")
                test = "One-way ANOVA"
                cor = summary(stats::aov(y ~ x, temp))

                cor$statistic = cor[[1]]$`F value`[1]
                cor$p.value = cor[[1]]$`Pr(>F)`[1]

              }
              else
              {
                cor = stats::t.test(met[non.met == level[1]], met[non.met == level[2]])
                test = "T-test"
              }

            }
            else
            {
              #Non-parametric test
              if(no.level > 2)
              {
                cor = stats::kruskal.test(met, non.met)
                test = "Kruskal-Wallis"
              }
              else
              {
                cor = stats::wilcox.test(met[non.met == level[1]], met[non.met == level[2]])
                test = "Mann-Whitney"
              }
            }

            temp = cbind.data.frame("X" = xname,
                                    "Y" = yname,
                                    "Type" = test,
                                    "Statistic" = cor$statistic,
                                    "pval" = cor$p.value,
                                    "no.data" = no.data)
            results = rbind.data.frame(results, temp)

            #box plot
            if(plot)
            {
              pl = data.frame(non.met, met)
              colnames(pl) = c(col.x, col.y)
              p1 = ggplot2::ggplot(pl, ggplot2::aes_string(x = col.x, y = col.y, fill = col.x)) + ggplot2::geom_boxplot()
              print(p1)
            }

          }
        },
        error = function(err){print(paste("Error", err, "with", xname, "and", yname))},
        warning = function(w){if(warning){print(paste("Warning", w, "with", xname, "and", yname))}}
      )
    }

  }
  row.names(results) = NULL
  return(results)
}
