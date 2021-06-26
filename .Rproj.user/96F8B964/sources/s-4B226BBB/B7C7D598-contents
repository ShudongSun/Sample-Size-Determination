#' @title Summarizes data
#' @description Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#'
#' @param data a data frame
#' @param measurevar the name of a column that contains the variable to be summariezed
#' @param groupvars a vector containing names of columns that contain grouping variables
#' @param na.rm a boolean that indicates whether to ignore NA's
#' @param conf.interval the percent range of the confidence interval (default is 95\%)
#' @param .drop should combinations of variables that do not appear in the input data be preserved (FALSE) or dropped (TRUE, default)
#'
#' @return the count, mean, standard deviation, standard error of the mean, and confidence interval of data
#' @export
#'
#' @examples
#'
#' mean_values = c(0,5)
#' sd_values = c(1,5)
#'
#' data<-data.frame(
#'     data.norm=c(rnorm(n=50, mean=0, sd=1),rnorm(n=50, mean=5, sd=1),rnorm(n=50, mean=0, sd=5),rnorm(n=50, mean=5, sd=5)),
#'          Mean_value=as.factor(rep(mean_values,each=50)),
#'               Sd_value=as.factor(rep(sd_values,each=100))
#'                )
#'
#' data.norm.sum<-summarySE(data,measurevar = "data.norm",groupvars = c("Mean_value","Sd_value"))
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,conf.interval=.95, .drop=TRUE)
{
  library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}
