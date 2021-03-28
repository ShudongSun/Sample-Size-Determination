#' @title Calculate the AUCs
#' @description The main function of SSD package: build the model and calculate the AUCs with parallel, you can also get the resulting plot via this function.
#'
#' @param n01_all size of all data labeled as class 0/1. For example, n01_all=c(800,800) represents that the size of all data labeled as class 0 is 800 and the size of all data labeled as class 1 is also 800.
#' @param n01_p size of pilot data labeled as class 0/1. For example, n01_p=c(15,15) represents that the size of pilot data labeled as class 0 is 15 and the size of pilot data labeled as class 1 is also 15.
#' @param n_train_sets size sets of training data labeled as class 0/1. For example, n_train_sets=c(c(30,30),c(90,90),c(150,150)) represents that we try 3 different sets of training data and the training size of the first set is c(30,30).
#' @param n01_test number of test data labeled as class 0/1. size of all data labeled as class 0/1. For example, n01_test=c(300,300) represents that the size of test data labeled as class 0 is 300 and the size of test data labeled as class 1 is also 300.
#' @param num_of_seeds number of seeds you want to use to run simulations.(If you want to make the plots, "num_of_seeds" must be not smaller than 2)
#' @param random_seeds
#' If TRUE, it will produce "num_of_seeds" seeds randomly.
#' If FASLE, it will use seeds "1:num_of_seeds", which is used to make it reproducible.
#' @param seeds
#' You can choose the seeds set as you wish. It's default value is NULL.
#' If it's not NULL, the function will use your seeds and ignore parameters "num_of_seeds" and "random_seeds".
#' @param calculate_std_of_AUC_and_produce_plot You can calculate the standard deviation of AUC and produce the plot directly if you set this parameter to TRUE. It will create a new folder named "result" in your current directory and save the plots into it.
#' @param method
#' Choose the method you want to use: "pca2_mvnorm" , "gaussian_copula" or "smote"(\code{\link[smotefamily]{SMOTE}}).
#' The default value is "pca2_mvnorm".
#' @param ncores
#' number of cores used for parallel computing. Default = detectCores() - 1.
#' @param model base classification model.
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
#' \item self: You can use your self-defined function. You need to pass your self-defined function via the "func" parameter.
#' }
#'
#' @param func If you set "model" to "self", you have to pass your self-defined model function. This function should be able to take "x_train" and "y_train" as the first two inputs to train the model and then take "x_test" as the third input and return the predicted scores of x_test data. For example, \cr\cr
#' \code{\cr
#' predict_model <- function(x_train, y_train, x_test){ \cr
#' data_trainxy<-data.frame(x_train,y_train=as.factor(y_train)) \cr
#' fit_svm<-svm(y_train~.,data=data_trainxy,probability=TRUE) \cr
#' pred_svm <- predict(fit_svm, x_test, probability=TRUE,decision.values = TRUE) \cr
#' p_svm=as.data.frame(attr(pred_svm, "probabilities"))$"1" \cr
#' return(p_svm) \cr
#' }\cr \cr
#' calculate_AUCs(n01_all= c(800,800), n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300), num_of_seeds=20, random_seeds=FALSE, model=c("self","randomforest"), func=predict_model, package_imported=c("e1071"), calculate_std_of_AUC_and_produce_plot=TRUE)}
#'
#' @param package_imported If you set "model" to "self",state the names of the packages you used in your "func", which have to be specified when using parallel computing.
#'
#' @param data_generation a parameter list that you can tell the function about the distribution and parameters you want to use to generate the data.
#' \itemize{
#' \item "gaussian" represent multivariate gaussian distribution. see \code{\link[MASS]{mvrnorm}} in \code{MASS} package. For example, data_generation=list(dist="gaussian",sigma=list(class_0=diag(5),class_1=diag(5)),mu=c(rep(0,5),rep(2,5)))
#' \item "t-distribution" represent multivariate t distribution. see \code{\link[mvtnorm]{rmvt}} in \code{mvtnorm} package. For example, data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5))).
#' }
#'
#'
#' @param data_input Its default value is NULL and the function will use the "data_generation" parameter to generate the data. If "data_input" is not NULL, the function will ignore the "data_generation" parameter and "n01_all" parameter, and use the "data_input" as the data.
#' Your "data_input" should be a list with "x_data" matrix and "y_data" matrix. For example, \cr\cr
#' \code{
#'
#'yeast_data <- read.table("./yeast.data") \cr}
#'###\link{https://archive.ics.uci.edu/ml/datasets/Diabetic+Retinopathy+Debrecen+Data+Set}\cr\cr
#'\code{
#'x_data = yeast_data[,c(2:5,8:9)]\cr
#'y_label = yeast_data[,10]\cr
#'id0 = which(y_label=="CYT" | y_label=="MIT")\cr
#'y_data = rep(1,length(y_label))\cr
#'y_data[id0] = 0\cr
#'table(y_data)\cr
#'data = list(x_data=x_data, y_data=y_data)\cr\cr
#'
#'calculate_AUCs(n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300), num_of_seeds=20, random_seeds=FALSE, calculate_std_of_AUC_and_produce_plot=TRUE, method="pca2_mvnorm", model=c("svm","randomforest"), data_input=data)\cr
#' }
#'
#' @return Return the AUCs you want to calculate
#' @export
#'
#' @examples
#' calculate_AUCs(n01_all= c(800,800), n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300), num_of_seeds=20, random_seeds=FALSE, calculate_std_of_AUC_and_produce_plot=TRUE, data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5))))
#'
calculate_AUCs <- function(n01_all= c(800,800), n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300), num_of_seeds=20, random_seeds=TRUE, seeds=NULL, calculate_std_of_AUC_and_produce_plot=FALSE, method="pca2_mvnorm", ncores = NULL, model=c("svm","randomforest"),func=NULL ,package_imported=NULL , data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5))),data_input=NULL)
{
  library(parallel)

  # print(data_generation)
  data_generation = data_generation

  fun <- function(x){
    return(calculate_AUC_base(n01_all=n01_all, n01_p=n01_p, n_train_sets=n_train_sets, n01_test=n01_test, seed=x, method=method, model=model,func=func, data_generation=data_generation, data_input=data_input ))
  }

  if(is.null(ncores)){
    clnum <- detectCores() - 1
  }else{
    clnum <- ncores
  }


  cl <- makeCluster(getOption("cl.cores", clnum));

  if(!is.null(package_imported)){
    clusterExport(cl, "package_imported", envir=environment())
    for (i in 1:length(package_imported)){
      clusterExport(cl, "i", envir=environment())
      clusterEvalQ(cl, library(package_imported[i],character.only = TRUE))
    }
  }
  clusterEvalQ(cl, library(SSD))
  clusterExport(cl, c("calculate_AUC_base"),envir=environment())
  if(!is.null(func)){
    clusterExport(cl, c("func"),envir=environment())
  }
  if(!is.null(data_input)){
    clusterExport(cl, "data_input", envir=environment())
  }

  if(is.null(seeds)){
    if(random_seeds==TRUE){
      seeds = sample(1:10000,num_of_seeds,replace=F)
    }
    else{
      seeds = 1:num_of_seeds
    }
  }

  system.time({
    res <- parLapply(cl, seeds,  fun)
  });

  stopCluster(cl)

  file = paste0("./auc_res.Rdata")
  save(res,file=file)

  # load("./auc_res.Rdata")
  print("res succeed!")

  if(calculate_std_of_AUC_and_produce_plot==TRUE){
    calculate_std_of_AUC_and_draw_plot(res,n_train_sets=n_train_sets, model=model)
  }

  return(res)

}
