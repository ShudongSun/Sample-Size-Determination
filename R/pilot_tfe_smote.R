#' @importFrom smotefamily SMOTE
#' @title smote
#' @description  Use SMOTE to generate data.
#' @param x_pilot input variables of pilot data
#' @param y_pilot labels of pilot data
#' @param n0_train the number of training data of class 0
#' @param n1_train the number of training data of class 1
#' @param n0_test the number of test data of class 0
#' @param n1_test the number of test data of class 1
#'
#' @return the generated data list including train_x, train_y and test_x
#' @export
#'
#' @examples
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
#'x0_all = rmvt(n = n0_all, sigma = covxx, delta = rep(0,d), df = df)
#'x1_all = rmvt(n = n1_all, sigma = covxx, delta = delta, df = df)
#'
#'x_data = rbind(x0_all,x1_all)
#'y_data = c(rep(0,n0_all),rep(1,n1_all))
#'
#'id0 <- which(y_data==0)
#'id1 <- which(y_data==1)
#'
#'id0_p <- sample(id0,n0_p)
#'id1_p <- sample(id1,n1_p)
#'id_p <- c(id0_p,id1_p)
#'
#'x_pilot <- as.matrix(x_data[id_p,])
#'y_pilot <- as.matrix(y_data[id_p])
#'
#'n1_train <- n0_train <- n_train <-60
#'n0_test <- n1_test <- 300
#'
#'data_list = pilot_tfe_smote(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test)
#'
pilot_tfe_smote <- function(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test)
{
  library(MASS)
  library(rlist)
  # library(smotefamily)

  # library(DMwR)

  id0_p = which(y_pilot==0)
  id1_p = which(y_pilot==1)

  n0_p = length(id0_p)
  n1_p = length(id1_p)

  n0_tt = n0_train + n0_test
  n1_tt = n1_train + n1_test

  x_pilot_df = data.frame(x_pilot)
  y_pilot_df = data.frame(y_pilot)
  pilot_df = cbind(x_pilot_df,y_pilot_df)
  # pilot_df$y_pilot = as.factor(pilot_df$y_pilot)
  # table(pilot_df$y_pilot)

  n_min_p = min(n0_p,n1_p)
  # n_min_tt = min(n0_tt,n1_tt)
  n_max_tt = max(n0_tt,n1_tt)

  # a = ceiling(n_min_tt/n_min_p-1)*100
  # b = ceiling(1e4*n_max_tt/a/n_min_p)
  label_col_num = grep("y_pilot",colnames(pilot_df))

  tempData1 <- SMOTE(pilot_df[,-label_col_num], pilot_df[,label_col_num], K=5, dup_size = ceiling(n_max_tt/n_min_p))
  tempData2 <- SMOTE(tempData1[["data"]][,-label_col_num], tempData1[["data"]][,label_col_num], K=5, dup_size = ceiling(n_max_tt/n_min_p))
  # newData <- SMOTE(y_pilot ~ ., pilot_df, perc.over = a, perc.under=b)
  newData = tempData2[["data"]]

  table(newData$class)

  id0 <- which(newData["class"]==0)
  id1 <- which(newData["class"]==1)

  ###train
  id0_train <- sample(id0,n0_train)
  id1_train <- sample(id1,n1_train)
  id_train <- c(id0_train,id1_train)

  x_train <- as.matrix(newData[,-grep("class",colnames(newData))][id_train,])
  y_train <- as.matrix(newData[,"class"][id_train])

  train_x=as.data.frame(x_train)
  train_y=y_train
  data_trainxy<-data.frame(train_x,y_train)

  ###test
  id0_remain = setdiff(id0,id0_train)
  id1_remain = setdiff(id1,id1_train)

  id0_test <- sample(id0_remain,n0_test)
  id1_test <- sample(id1_remain,n1_test)
  id_test <- c(id0_test,id1_test)

  x_test <- as.matrix(newData[,-grep("class",colnames(newData))][id_test,])
  y_test <- as.matrix(newData[,"class"][id_test])

  test_x=as.data.frame(x_test)
  # test_y=as.data.frame(y_test)

  data_list<-list(train_x=train_x,train_y=y_train,test_x=test_x)

  return(data_list)

}
