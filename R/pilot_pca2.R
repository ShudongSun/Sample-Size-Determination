#' @title Do the PC2
#' @description  Use PCA and select the first two PCs to reduce the dimension, then get data of the first two PCs.
#' @param x_train input variables of training data
#' @param y_train labels of training data
#' @param x_test input variables of test data
#'
#' @return the generated first two PC data list including train_x, train_y and test_x
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
#' n1_train <- n0_train <- n_train <-60
#' n0_test <- n1_test <- 300
#'
#'x0_all = rmvt(n = n0_all, sigma = covxx, delta = rep(0,d), df = df)
#'x1_all = rmvt(n = n1_all, sigma = covxx, delta = delta, df = df)
#'
#'x_data = rbind(x0_all,x1_all)
#'y_data = c(rep(0,n0_all),rep(1,n1_all))
#'
#'id0 <- which(y_data==0)
#'id1 <- which(y_data==1)
#'
#'id0_train <- sample(id0,n0_train)
#'id1_train <- sample(id1,n1_train)
#'id_train <- c(id0_train,id1_train)
#'x_train <- as.matrix(x_data[id_train,])
#'y_train <- as.matrix(y_data[id_train])
#'
#'id0_remain = setdiff(id0,id0_train)
#'id1_remain = setdiff(id1,id1_train)
#'
#'id0_test <- sample(id0_remain,n0_test)
#'id1_test <- sample(id1_remain,n1_test)
#'id_test <- c(id0_test,id1_test)
#'x_test <- as.matrix(x_data[id_test,])
#'
#'data_list = pilot_pca2(x_train,y_train,x_test)
#'
pilot_pca2 <- function(x_train,y_train,x_test)
{
  x_train.pca = prcomp(x_train,center = FALSE,scale. = FALSE)

  x_pc1 = x_train.pca$x[,1]
  x_pc2 = x_train.pca$x[,2]

  pc_p1=x_train.pca$rotation[,1]
  pc_p2=x_train.pca$rotation[,2]

  x_train_2pc = as.matrix(cbind(x_pc1,x_pc2))

  train_x=as.data.frame(x_train_2pc)
  names(train_x) <- c("pc1", "pc2")
  train_y = y_train

  test_x0_pc1 = as.matrix(x_test)%*%pc_p1
  test_x1_pc2 = as.matrix(x_test)%*%pc_p2

  test_x_pc2 = as.matrix(cbind(test_x0_pc1,test_x1_pc2))
  test_x=as.data.frame(test_x_pc2)
  names(test_x) <- c("pc1", "pc2")


  data_list<-list(train_x=train_x,train_y=train_y,test_x=test_x)

  return(data_list)

}
