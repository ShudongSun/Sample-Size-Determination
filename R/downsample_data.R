#' @title Downsample Data
#' @description Downsample the training data.
#' @param data_list data list you want to downsample(including train_x, train_y and test_x)
#' @param n0_train
#' Int, represents the absolute number of class 0 training data.
#' @param n1_train
#' Int, represents the absolute number of class 1 training data.
#'
#' @return the downsampled data
#' @export
#'
#' @examples
#'
#' library(mvtnorm)
#' library(MASS)
#'
#' df = 10
#' rho = 0.5
#' d = 5
#' delta = rep(2, d)
#' H <- abs(outer(1:d, 1:d, "-"))
#' covxx = rho^H
#'
#' n1_all <- n0_all <- 800
#' n1_p <- n0_p <- 15
#'
#' x0_all = rmvt(n = n0_all, sigma = covxx, delta = rep(0, d), df = df)
#' x1_all = rmvt(n = n1_all, sigma = covxx, delta = delta, df = df)
#'
#' x_data = rbind(x0_all, x1_all)
#' y_data = c(rep(0, n0_all), rep(1, n1_all))
#'
#' id0 <- which(y_data == 0)
#' id1 <- which(y_data == 1)
#'
#' id0_p <- sample(id0, n0_p)
#' id1_p <- sample(id1, n1_p)
#' id_p <- c(id0_p, id1_p)
#'
#' x_pilot <- as.matrix(x_data[id_p, ])
#' y_pilot <- as.matrix(y_data[id_p])
#'
#' n1_train <- n0_train <- n_train <- 150
#' n0_test <- n1_test <- 300
#'
#' data_list = pilot_tfe_mvnorm_pca2(x_pilot, y_pilot, n0_train, n1_train, n0_test, n1_test)
#'
#' data_list_ds = downsample_data(data_list, n0_train=30, n1_train=30, seed=1)
#'

downsample_data <- function(data_list, n0_train, n1_train, seed=NULL)
{
  x_train = data_list$train_x
  y_train = data_list$train_y
  x_test = data_list$test_x

  id0 <- which(y_train==0)
  id1 <- which(y_train==1)

  n0 = length(id0)
  n1 = length(id1)
  # check n_all ?= length(y_data)

  if(!is.null(seed)){
    set.seed(seed)
  }

  if(n0_train > n0 || n1_train > n1){
    stop('downsample data error: input training data in the data list are not sufficient to downsample!')
  }

  id0_ds <- sample(id0,n0_train)
  id1_ds <- sample(id1,n1_train)
  id_ds <- c(id0_ds,id1_ds)

  train_x <- as.data.frame(x_train[id_ds,])
  train_y <- y_train[id_ds]


  result = list(train_x=train_x, train_y=train_y, test_x=x_test)
  return(result)

}
