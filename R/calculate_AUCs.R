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
#' Choose the method you want to use: "pca2_mvnorm" and "gaussian_copula".
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
#' }
#' @param data_generation a parameter list that you can tell the function about the distribution and parameters you want to use to generate the data.
#' \itemize{
#' \item "gaussian" represent multivariate gaussian distribution. see \code{\link[MASS]{mvrnorm}} in \code{MASS} package. For example, data_generation=list(dist="gaussian",sigma=list(class_0=diag(5),class_1=diag(5)),mu=c(rep(0,5),rep(2,5)))
#' \item "t-distribution" represent multivariate t distribution. see \code{\link[mvtnorm]{rmvt}} in \code{mvtnorm} package. For example, data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5))).
#' }
#' @return Return the AUCs you want to calculate
#' @export
#'
#' @examples
#' calculate_AUCs(n01_all= c(800,800), n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300), num_of_seeds=20, random_seeds=FALSE, calculate_std_of_AUC_and_produce_plot=TRUE)
#'
calculate_AUCs <- function(n01_all= c(800,800), n01_p=c(15,15), n_train_sets = c(c(15,15),c(30,30),c(60,60),c(120,120),c(150,150)), n01_test=c(300,300), num_of_seeds=20, random_seeds=TRUE, seeds=NULL, calculate_std_of_AUC_and_produce_plot=FALSE, method="pca2_mvnorm", ncores = NULL, model=c("svm","randomforest"), data_generation=list(dist="t-distribution",sigma=list(class_0=diag(5),class_1=diag(5)),df=c(10,10),delta=c(rep(0,5),rep(2,5))))
{
  library(parallel)

  # print(data_generation)
  data_generation = data_generation

  fun <- function(x){
    return(calculate_AUC_base(n01_all=n01_all, n01_p=n01_p, n_train_sets=n_train_sets, n01_test=n01_test, seed=x, method=method, model=model, data_generation=data_generation ))
  }

  if(is.null(ncores)){
    clnum <- detectCores() - 1
  }else{
    clnum <- ncores
  }


  cl <- makeCluster(getOption("cl.cores", clnum));

  clusterEvalQ(cl, c(library(SSD)))
  clusterExport(cl, c("calculate_AUC_base"),
                envir=environment())

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

  if(calculate_std_of_AUC_and_produce_plot==TRUE){
    calculate_std_of_AUC_and_draw_plot(res,n_train_sets=n_train_sets, model=model)
  }

  return(res)

}
