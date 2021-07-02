#' @title Stardard the input data
#' @description make the input data standard and ready to train
#' @param x_train input variables of training data
#' @param y_train labels of training data
#' @param x_test input variables of test data
#'
#' @return the standard data list including train_x, train_y and test_x
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
#'result = pilot_n(x_train,y_train,x_test)
pilot_n <- function(x_train,y_train,x_test)
{

  data_trainxy<-data.frame(x_train,y_train)

  num_of_variables = length(x_test[1,])
  test_x=as.data.frame(x_test)
  if(is.null(colnames(x_train))){
    cnames = paste("X",1:num_of_variables,sep="")
  }else{
    cnames = colnames(x_train)
  }
  colnames(test_x)=cnames


  data_list<-list(train_x=x_train,train_y=y_train,test_x=test_x)

  return(data_list)

}
