#' Title Calculate the AUCs
#'
#' @param n01_p number of pilot data labeled as class 0/1
#' @param n_train_sets size sets of training data labeled as class 0/1
#' @param n01_test number of test data labeled as class 0/1
#' @param num_of_seeds number of seeds
#' @param random_seeds
#' If TRUE, it will produce "num_of_seeds" seeds randomly.
#' If FASLE, it will use seeds "1:num_of_seeds", which is used to make it reproducible.
#' @param seeds
#' You can choose the seeds set as you wish. It's default value is NULL.
#' If it's not NULL, the function will use your seeds and ignore parameters "num_of_seeds" and "random_seeds".
#' @param calculate_std_of_AUC_and_produce_plot You can calculate the standard deviation of AUC and produce the plot directly if you set this parameter to TRUE.
#' @param method
#' Choose the method you want to use: "pca2_mvnorm" and "gaussian_copula".
#' The default value is "pca2_mvnorm".
#' @param ncores
#' number of cores used for parallel computing. Default = detectCores() - 1.
#'
#' @return Return the AUCs you want to calculate
#' @export
#'
#' @examples
#' calculate_AUCs(n01_p=15, n01_test=300, num_of_seeds=20, random_seeds=TRUE)
#' calculate_AUCs(n01_p=15, n01_test=300, num_of_seeds=20, random_seeds=FALSE)
#' calculate_AUCs(n01_p=15, n01_test=300, seeds=c(2,4,6,7,9,12,25,34,24,65))
#' calculate_AUCs(n01_p=15, n01_test=300, num_of_seeds=20, random_seeds=TRUE, calculate_std_of_AUC_and_produce_plot=TRUE)
#'
calculate_AUCs <- function(n01_p=15, n_train_sets = c(15,30,60,120,150), n01_test=300, num_of_seeds=20, random_seeds=TRUE, seeds=NULL, calculate_std_of_AUC_and_produce_plot=FALSE, method="pca2_mvnorm", ncores = NULL)
{
  library(parallel)

  fun <- function(x){
    return(calculate_AUC_base(n01_p=n01_p, n_train_sets=n_train_sets, n01_test=n01_test, seed=x, method=method))
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
    calculate_std_of_AUC_and_draw_plot(res,n_train_sets = n_train_sets)
  }

  return(res)

}
