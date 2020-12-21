#' @title Estimation from Gaussian Copula
#' @description  Use Gaussian Copula as the generative model and train different models.
#' @param x_pilot input variables of pilot data
#' @param y_pilot labels of pilot data
#' @param n0_train the number of training data of class 0
#' @param n1_train the number of training data of class 1
#' @param n0_test the number of test data of class 0
#' @param n1_test the number of test data of class 1
#' @param method base classification method.
#' \itemize{
#' \item logistic: Logistic regression. \link{glm} function with family = 'binomial'
#' \item penlog: Penalized logistic regression with LASSO penalty. \code{\link[glmnet]{glmnet}} in \code{glmnet} package
#' \item svm: Support Vector Machines. \code{\link[e1071]{svm}} in \code{e1071} package
#' \item randomforest: Random Forest. \code{\link[randomForest]{randomForest}} in \code{randomForest} package
#' \item lda: Linear Discriminant Analysis. \code{\link[MASS]{lda}} in \code{MASS} package
#' \item slda: Sparse Linear Discriminant Analysis with LASSO penalty.
#' \item nb: Naive Bayes. \code{\link[e1071]{naiveBayes}} in \code{e1071} package
#' \item nnb: Nonparametric Naive Bayes. \code{\link[naivebayes]{naive_bayes}} in \code{naivebayes} package
#' \item ada: Ada-Boost. \code{\link[ada]{ada}} in \code{ada} package
#' \item xgboost: XGBboost. \code{\link[xgboost]{xgboost}} in \code{xgboost} package
#' \item tree: Classificatin Tree. \code{\link[tree]{tree}} in \code{tree} package
#' }
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
#'result = pilot_tfe_gaussian_copula(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test,method=c("svm","randomforest"))
#'
#'
#'
pilot_tfe_gaussian_copula <- function(x_pilot,y_pilot,n0_train,n1_train,n0_test,n1_test,method=c("svm","randomforest"))
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

  ###generate the samples based on the estimated distributions
  n_train=n0_train+n1_train
  n_test=n0_test+n1_test


  ###y
  train_y=c(rep(0,n0_train),rep(1,n1_train))

  ###train
  train_x0=rMvdc(n0_train, mymvd0)
  train_x1=rMvdc(n1_train, mymvd1)
  trx=matrix(rbind(train_x0,train_x1),n_train,num_feature)
  train_x=as.data.frame(trx)

  ###test
  test_x0=rMvdc(n0_test, mymvd0)
  test_x1=rMvdc(n1_test, mymvd1)
  tex=matrix(rbind(test_x0,test_x1),n_test,num_feature)
  test_x=as.data.frame(tex)

  data_trainxy<-data.frame(train_x,train_y)

  methods_all = c("logistic", "penlog", "svm", "randomforest", "lda", "slda", "nb", "nnb", "ada", "tree","xgboost")

  p_results = numeric()
  # print(method)
  for (i in 1:length(method)){
    # print(method[i])
    if(!method[i] %in% methods_all){
      stop('method \'',method[i], '\' cannot be found')
    }

    ###LR
    if(method[i] == "logistic"){
      fit_LR<-suppressWarnings(glm(train_y~.,family = "binomial",data=data_trainxy, maxit=100))
      prep_LR<-predict(fit_LR,test_x)
      p_LR<-1/(1+exp(-prep_LR))
      p_results=rbind(p_results,p_LR)
    }

    ###xgboost
    if(method[i] == "xgboost"){
      new_trainx<-as.matrix(train_x)
      dx_trainy<-xgb.DMatrix(data = new_trainx, label = train_y)
      fit_xgb <- xgboost(data=dx_trainy, nthread=3,nrounds=100,objective = "binary:logistic", verbose = 0)
      p_xgb<-predict(fit_xgb,as.matrix(test_x))
      p_results=rbind(p_results,p_xgb)
    }

    ###SVM
    if(method[i] == "svm"){
      data_trainxy<-data.frame(train_x,train_y=as.factor(train_y))
      fit_svm<-svm(train_y~.,data=data_trainxy,probability=TRUE)
      pred_svm <- predict(fit_svm, test_x, probability=TRUE,decision.values = TRUE)
      p_svm=as.data.frame(attr(pred_svm, "probabilities"))$"1"
      p_results=rbind(p_results,p_svm)
    }

    ###RF
    if(method[i] == "randomforest"){
      data_trainxy<-data.frame(train_x,train_y=as.factor(train_y))
      fit_RF<-randomForest(train_y~.,data = data_trainxy,importance=TRUE)
      p_RF=predict(fit_RF,test_x,type = "prob")[, 2]
      p_results=rbind(p_results,p_RF)
    }

    ###Penalized logistic regression with LASSO penalty
    if(method[i] == "penlog"){
      fit_penlog = cv.glmnet(as.matrix(train_x), train_y, family = "binomial")
      p_penlog = t(predict(fit_penlog$glmnet.fit, newx = as.matrix(test_x), type = "response", s = fit_penlog$lambda.min))
      rownames(p_penlog)="p_penlog"
      p_results=rbind(p_results,p_penlog)
    }

    ###Linear Discriminant Analysis
    if(method[i] == "lda"){
      fit_lda = lda(as.matrix(train_x), train_y)
      p_lda = predict(fit_lda, as.matrix(test_x))$posterior[, 2]
      p_results=rbind(p_results,p_lda)
    }

    ###Sparse Linear Discriminant Analysis with LASSO penalty
    if(method[i] == "slda"){
      n1 = sum(train_y==1)
      n0 = sum(train_y==0)
      n = n1 + n0
      y_lda = train_y
      y_lda[train_y == 0] = -n/n0
      y_lda[train_y == 1] = n/n1
      fit_slda = cv.glmnet(as.matrix(train_x), y_lda)
      score_slda = t(predict(fit_slda$glmnet.fit, newx = as.matrix(test_x), type = "link", s = fit_slda$lambda.min))
      p_slda = 1/(1+exp(-score_slda))
      rownames(p_slda)="p_slda"
      p_results=rbind(p_results,p_slda)
    }

    ###Naive Bayes
    if(method[i] == "nb"){
      train_data_nb = data.frame(train_x, y = train_y)
      fit_nb <- naive_bayes(as.factor(y) ~ ., data = train_data_nb, usekernel = FALSE)
      p_nb = predict(fit_nb, data.frame(test_x), type = "prob")[,2]
      p_results=rbind(p_results,p_nb)
    }

    ###Nonparametric Naive Bayes
    if(method[i] == "nnb"){
      train_data_nnb = data.frame(train_x, y = train_y)
      fit_nnb <- naive_bayes(as.factor(y) ~ ., data = train_data_nnb, usekernel = TRUE)
      p_nnb = predict(fit_nnb, data.frame(test_x), type = "prob")[,2]
      p_results=rbind(p_results,p_nnb)
    }

    ###Ada-Boost
    if(method[i] == "ada"){
      train_data_ada = data.frame(train_x, y = train_y)
      fit_ada = ada(y ~ ., data = train_data_ada)
      p_ada = predict(fit_ada, data.frame(test_x), type = "probs")[, 2]
      p_results=rbind(p_results,p_ada)
    }

    ###Classification
    if(method[i] == "tree"){
      # train_y = as.factor(train_y)
      train_data_tree = data.frame(train_x, y = train_y)
      fit_tree = tree(y~ ., data = train_data_tree)
      p_tree = predict(fit_tree, newdata = data.frame(test_x), type = 'vector')
      p_results=rbind(p_results,p_tree)
    }

  }

  return(p_results)

}
