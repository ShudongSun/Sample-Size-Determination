#' @title Estimation from Gaussian Copula
#' @description  Use Gaussian Copula as the generative model and train 4 different models.
#' @details Use Logistic Regresion, Random Forrest, Support Vector Machine and Xgboost as training model.
#' @param x_pilot input variables of pilot data
#' @param y_pilot labels of pilot data
#' @param n0_train the number of training data of class 0
#' @param n1_train the number of training data of class 1
#' @param n0_test the number of test data of class 0
#' @param n1_test the number of test data of class 1
#'
#' @return he scores predicted by Logistic Regresion, Random Forrest, Support Vector Machine and Xgboost seperately
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
#'result = pilot_tfe_gaussian_copula(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test)
#'
#'
#'
pilot_tfe_gaussian_copula <- function(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test)
{
  library(copula)
  library(MASS)
  library(rlist)

  id0_p = which(y_pilot==0)
  id1_p = which(y_pilot==1)
  num_feature = length(x_pilot[id0_p[1],])

  mu0_hat = rep(0,num_feature)
  mu1_hat = rep(0,num_feature)
  sd_hat = rep(0,num_feature)

  para0_list = list()
  para1_list = list()

  # sd1_hat = rep(0,num_feature)
  for (i in 1:num_feature){
    mu0_hat[i] = mean(x_pilot[id0_p,i])
    mu1_hat[i] = mean(x_pilot[id1_p,i])
    sd_hat[i] = sqrt(((num_feature-1)*var(x_pilot[id0_p,i])+(num_feature-1)*var(x_pilot[id1_p,i]))/(num_feature+num_feature-2))
    para0_list = list.append(para0_list,list(mean=mu0_hat[i],sd=sd_hat[i]))
    para1_list = list.append(para1_list,list(mean=mu1_hat[i],sd=sd_hat[i]))
  }

  # cor_param = as.numeric(cor(x_pilot[id0_p,], method = "kendall"))
  m <- pobs(as.matrix(x_pilot))
  mycop <- normalCopula(dim = num_feature, dispstr = "un")
  fit <- fitCopula(mycop, m, method = 'ml')
  cor_param = coef(fit)

  # selectedCopula <- BiCopSelect(m, familyset = NA)

  mycop <- normalCopula(param = cor_param, dim = num_feature, dispstr = "un")
  mymvd0 <- mvdc(copula=mycop,margins=rep("norm",num_feature),paramMargins=para0_list)
  mymvd1 <- mvdc(copula=mycop,margins=rep("norm",num_feature),paramMargins=para1_list)

  ######################generate the samples based on the estimated distributions
  n_train=n0_train+n1_train
  n_test=n0_test+n1_test


  ###########################y
  train_y=c(rep(0,n0_train),rep(1,n1_train))
  # y0index=which(train_y==0)
  # y1index=which(train_y==1)



  ######################train
  train_x0=rMvdc(n0_train, mymvd0)
  train_x1=rMvdc(n1_train, mymvd1)
  trx=matrix(rbind(train_x0,train_x1),n_train,num_feature)
  train_x=as.data.frame(trx)

  #########################test
  test_x0=rMvdc(n0_test, mymvd0)
  test_x1=rMvdc(n1_test, mymvd1)
  tex=matrix(rbind(test_x0,test_x1),n_test,num_feature)
  test_x=as.data.frame(tex)

  data_trainxy<-data.frame(train_x,train_y)

  #ind0 = which(test_y==0)
  #ind1 = which(test_y==1)


  ################################LR
  ############LR
  fit.LR<-glm(train_y~.,family = "binomial",data=data_trainxy)
  prep_LR<-predict(fit.LR,test_x)
  p_LR<-1/(1+exp(-prep_LR))


  ########xgboost
  library(xgboost)
  new_trainx<-as.matrix(train_x)
  dtrain_xy<-xgb.DMatrix(data = new_trainx, label = train_y)
  xgb <- xgboost(data=dtrain_xy, nthread=3,nrounds=100,objective = "binary:logistic", verbose = 0)
  p_xgb<-predict(xgb,as.matrix(test_x))



  #############SVM
  library(e1071)
  data_trainxy<-data.frame(train_x,train_y=as.factor(train_y))
  fit.svm<-svm(train_y~.,data=data_trainxy,probability=TRUE)
  pred_svm <- predict(fit.svm, test_x, probability=TRUE,decision.values = TRUE)
  p_svm=attr(pred_svm, "probabilities")[,2]



  #############RF
  library(randomForest)
  data_trainxy<-data.frame(train_x,train_y=as.factor(train_y))
  fit.RF<-randomForest(train_y~.,data = data_trainxy,importance=TRUE)
  p_RF=predict(fit.RF,test_x,type = "prob")[, 2]






  result=list(p.LR=p_LR,p.xgb=p_xgb,p.svm=p_svm,p.RF=p_RF)
  return(result)

}
