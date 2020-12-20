#' @title Generate Data
#'
#' @param seed the seed number
#' @param n01_all size of all data labeled as class 0/1. For example, n01_all=c(800,800) represents that the size of all data labeled as class 0 is 800 and the size of all data labeled as class 1 is also 800.
#'
#' @return the generated data
#' @export
#'
#' @examples generate_data(seed=1)
generate_data <- function(seed=1, n01_all=c(800,800))
{
  library(mvtnorm)

  df = 10
  rho=0.5
  d=5
  delta = rep(2,d)
  H<-abs(outer(1:d,1:d,"-"))
  covxx=rho^H
  n0_all <- n01_all[1]
  n1_all <- n01_all[2]

  set.seed(seed)

  x0_all = rmvt(n = n0_all, sigma = covxx, delta = rep(0,d), df = df)
  x1_all = rmvt(n = n1_all, sigma = covxx, delta = delta, df = df)

  x_data = rbind(x0_all,x1_all)
  y_data = c(rep(0,n0_all),rep(1,n1_all))

  result = list(x_data=x_data, y_data=y_data)
  return(result)
}
