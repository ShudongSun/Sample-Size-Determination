#' @title Generate Data
#'
#' @param seed the seed number
#'
#' @return the generated data
#' @export
#'
#' @examples generate_data(seed=1)
generate_data <- function(seed=1)
{
  library(mvtnorm)

  n1_p <- n0_p <- 15
  n_p=n0_p+n1_p

  df = 10
  rho=0.5
  d=5
  delta = rep(2,d)
  H<-abs(outer(1:d,1:d,"-"))
  covxx=rho^H
  n1_all <- n0_all <- 800

  n0_test <- n1_test <- 300
  n_test = n0_test + n1_test

  set.seed(seed)

  x0_all = rmvt(n = n0_all, sigma = covxx, delta = rep(0,d), df = df)
  x1_all = rmvt(n = n1_all, sigma = covxx, delta = delta, df = df)

  x_data = rbind(x0_all,x1_all)
  y_data = c(rep(0,n0_all),rep(1,n1_all))

  result = list(x_data=x_data, y_data=y_data)
  return(result)
}
