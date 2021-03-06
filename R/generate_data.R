#' @title Generate Data
#'
#' @param seed the seed number
#' @param n01_all size of all data labeled as class 0/1. For example, n01_all=c(800,800) represents that the size of all data labeled as class 0 is 800 and the size of all data labeled as class 1 is also 800.
#' @param data_generation a parameter list that you can tell the function about the distribution and parameters you want to use to generate the data.
#' \itemize{
#' \item "gaussian" represent multivariate gaussian distribution. see \code{\link[MASS]{mvrnorm}} in \code{MASS} package. For example, data_generation=list(dist="gaussian",sigma=list(class_0=diag(5),class_1=diag(5)),mu=c(rep(0,5),rep(2,5)))
#' \item "t-distribution" represent multivariate t distribution. see \code{\link[mvtnorm]{rmvt}} in \code{mvtnorm} package. For example, data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5)))
#' }
#'
#' @return the generated data
#' @export
#'
#' @examples
#' df = 10
#' rho=0.5
#' d=5
#' delta = rep(2,d)
#' H<-abs(outer(1:d,1:d,"-"))
#' covxx=rho^H
#' data = generate_data(seed=1, n01_all=c(800,800),data_generation=list(dist="t-distribution",sigma=list(class_0=covxx,class_1=covxx),df=c(10,10),delta=c(rep(0,5),rep(2,5))))
generate_data <- function(seed=1, n01_all=c(800,800),data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5))))
{
  library(mvtnorm)
  library(MASS)


  n0_all <- n01_all[1]
  n1_all <- n01_all[2]

  set.seed(seed)



  if(data_generation$dist=="t-distribution"){
    delta = data_generation$delta
    dim(delta) = c(5,2)
    delta = t(delta)

    x0_all = rmvt(n = n0_all, sigma = data_generation$sigma$class_0, delta = delta[1,], df = data_generation$df[1])
    x1_all = rmvt(n = n1_all, sigma = data_generation$sigma$class_1, delta = delta[2,], df = data_generation$df[2])
  }

  if(data_generation$dist=="gaussian"){
    mu = data_generation$mu
    dim(mu) = c(5,2)
    mu = t(mu)

    x0_all = mvrnorm(n = n0_all, mu=mu[1,], Sigma=data_generation$sigma$class_0)
    x1_all = mvrnorm(n = n1_all, mu=mu[2,], Sigma=data_generation$sigma$class_1)
  }



  x_data = rbind(x0_all,x1_all)
  y_data = c(rep(0,n0_all),rep(1,n1_all))

  result = list(x_data=x_data, y_data=y_data)
  return(result)
}
