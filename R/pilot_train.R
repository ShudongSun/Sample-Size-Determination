#' @title Training the data directly
#' @description  use 4 different methods to train the data.
#' @details Use Logistic Regresion, Random Forrest, Support Vector Machine and Xgboost as training model.
#' @param x_train input variables of training data
#' @param y_train labels of training data
#' @param x_test input variables of test data
#'
#' @return the scores predicted by Logistic Regresion, Random Forrest, Support Vector Machine and Xgboost seperately
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
#'result = pilot_train(x_train,y_train,x_test)
pilot_train <- function(x_train,y_train,x_test)
{

  data_trainxy<-data.frame(x_train,y_train)

  num_of_variables = length(x_test[1,])
  test_x=as.data.frame(x_test)
  cnames=paste("X",1:num_of_variables,sep="")
  colnames(test_x)=cnames


  ################################LR
  ############LR
  fit.LR<-suppressWarnings(glm(y_train~.,family = "binomial",data=data_trainxy, maxit=100))
  prep_LR<-predict(fit.LR,test_x)
  p_LR<-1/(1+exp(-prep_LR))


  ########xgboost
  library(xgboost)
  new_trainx<-as.matrix(x_train)
  dx_trainy<-xgb.DMatrix(data = new_trainx, label = y_train)
  xgb <- xgboost(data=dx_trainy, nthread=3,nrounds=100,objective = "binary:logistic", verbose = 0)
  p_xgb<-predict(xgb,as.matrix(test_x))


  #############SVM
  library(e1071)
  data_trainxy<-data.frame(x_train,y_train=as.factor(y_train))
  fit.svm<-svm(y_train~.,data=data_trainxy,probability=TRUE)
  pred_svm <- predict(fit.svm, test_x, probability=TRUE,decision.values = TRUE)
  p_svm=as.data.frame(attr(pred_svm, "probabilities"))$"1"



  #############RF
  library(randomForest)
  data_trainxy<-data.frame(x_train,y_train=as.factor(y_train))
  fit.RF<-randomForest(y_train~.,data = data_trainxy,importance=TRUE)
  p_RF=predict(fit.RF,test_x,type = "prob")[, 2]



  result=list(p.LR=p_LR,p.xgb=p_xgb,p.svm=p_svm,p.RF=p_RF)
  return(result)

}
