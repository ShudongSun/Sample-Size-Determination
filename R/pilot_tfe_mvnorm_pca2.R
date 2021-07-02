#' @title Get estimation from Pilot Data using PCA_2 and Mvnorm
#' @description  Use PCA and select the first two PCs to reduce the dimension, then use multivariate normal distribution as the generative model and generate data.
#' @param x_pilot input variables of pilot data
#' @param y_pilot labels of pilot data
#' @param n0_train the number of training data of class 0
#' @param n1_train the number of training data of class 1
#' @param n0_test the number of test data of class 0
#' @param n1_test the number of test data of class 1
#'
#'
#' @return the generated data list including train_x, train_y and test_x
#' @export
#'
#' @examples
#'
#' library(mvtnorm)
#' library(MASS)
#'
#' df = 10
#' rho=0.5
#' d=5
#' delta = rep(2,d)
#' H<-abs(outer(1:d,1:d,"-"))
#' covxx=rho^H
#'
#' n1_all <- n0_all <- 800
#' n1_p <- n0_p <- 15
#'
#' x0_all = rmvt(n = n0_all, sigma = covxx, delta = rep(0,d), df = df)
#' x1_all = rmvt(n = n1_all, sigma = covxx, delta = delta, df = df)
#'
#' x_data = rbind(x0_all,x1_all)
#' y_data = c(rep(0,n0_all),rep(1,n1_all))
#'
#' id0 <- which(y_data==0)
#' id1 <- which(y_data==1)
#'
#' id0_p <- sample(id0,n0_p)
#' id1_p <- sample(id1,n1_p)
#' id_p <- c(id0_p,id1_p)
#'
#' x_pilot <- as.matrix(x_data[id_p,])
#' y_pilot <- as.matrix(y_data[id_p])
#'
#' n1_train <- n0_train <- n_train <-60
#' n0_test <- n1_test <- 300
#'
#' data_list = pilot_tfe_mvnorm_pca2(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test)
#'
#'
#'
pilot_tfe_mvnorm_pca2 <- function(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test)
{
  library(MASS)

  id0_p = which(y_pilot==0)
  id1_p = which(y_pilot==1)

  n0_p = length(id0_p)
  n1_p = length(id1_p)


  ### pca
  x_pilot.pca = prcomp(x_pilot,center = FALSE,scale. = FALSE)
  x0_pc1 = x_pilot.pca$x[id0_p,1]
  x1_pc1 = x_pilot.pca$x[id1_p,1]

  x0_pc2 = x_pilot.pca$x[id0_p,2]
  x1_pc2 = x_pilot.pca$x[id1_p,2]

  pc_p1=x_pilot.pca$rotation[,1]
  pc_p2=x_pilot.pca$rotation[,2]

  ### estimate the paramters: mean, variance
  mu0_hat1 = mean(x0_pc1)
  mu1_hat1 = mean(x1_pc1)

  mu0_hat2 = mean(x0_pc2)
  mu1_hat2 = mean(x1_pc2)

  mu0_hat = c(mu0_hat1,mu0_hat2)
  mu1_hat = c(mu1_hat1,mu1_hat2)

  x0_pc = cbind(x0_pc1,x0_pc2)
  cov0 = cov(x0_pc)
  x1_pc = cbind(x1_pc1,x1_pc2)
  cov1 = cov(x1_pc)

  sigma_hat = ((n0_p-1)*cov0+(n1_p-1)*cov1)/(n0_p+n1_p-2)


  ### generate the samples based on the estimated distributions
  n_train=n0_train+n1_train
  n_test=n0_test+n1_test


  ### y
  train_y=c(rep(0,n0_train),rep(1,n1_train))
  y0index=which(train_y==0)
  y1index=which(train_y==1)


  ### train
  train_x0=t(mvrnorm(n0_train, mu0_hat, sigma_hat))
  train_x1=t(mvrnorm(n1_train, mu1_hat, sigma_hat))
  trx=matrix(t(cbind(train_x0,train_x1)),n_train,2)
  train_x=as.data.frame(trx)

  ### test
  test_x0=t(mvrnorm(n0_test, mu0_hat, sigma_hat))
  test_x1=t(mvrnorm(n1_test, mu1_hat, sigma_hat))
  tex=matrix(t(cbind(test_x0,test_x1)),n_test,2)
  test_x=as.data.frame(tex)

  data_list<-list(train_x=train_x,train_y=train_y,test_x=test_x)

  return(data_list)

}
