#' Title Main
#'
#' @param n01_p size of pilot data labeled as class 0/1
#' @param n_train_sets size sets of training data labeled as class 0/1
#' @param n01_test size of test data labeled as class 0/1
#' @param seed random seed number
#' @param method
#' Choose the method you want to use: "pca2_mvnorm" and "gaussian_copula".
#' The default value is "pca2_mvnorm".
#'
#' @return AUC of ...
#' @export
#'
#' @examples AUC = calculate_AUC(n01_p=15, n01_test=300, seed=1)
calculate_AUC_base <- function(n01_p=15, n_train_sets = c(15,30,60,120,150), n01_test=300, seed=1, method="pca2_mvnorm")
{
  library(PRROC)
  n1_p <- n0_p <- n01_p
  n_p=n0_p+n1_p

  n0_test <- n1_test <- n01_test
  n_test = n0_test + n1_test

  data = generate_data(seed=seed)
  pilot_rest_data = split_data(data$x_data, data$y_data, n_train=n_p, seed=seed)


  test_y=c(rep(0,n0_test),rep(1,n1_test))

  n_train_sets = n_train_sets
  Loop=100

  LR.auc <- RF.auc <- svm.auc <- xgb.auc <- matrix(rep(0,length(n_train_sets)*Loop*2),length(n_train_sets)*2)
  count = 1
  for (test_from_true in c(0,1)){
    for (n01_train in n_train_sets){
      n1_train <- n0_train <- n01_train
      n_train = n0_train + n1_train

      p.LR <- p.RF <- p.svm <- p.xgb <- matrix(0,Loop,n_test)
      for (L in 1:Loop)
      {
        if(test_from_true==0){
          if(method == "pca2_mvnorm"){result = pilot_tfe_mvnorm_pca2(pilot_rest_data$x_train,pilot_rest_data$y_train,n0_train,n1_train,n0_test,n1_test)}
          if(method == "gaussian_copula"){result = pilot_tfe_gaussian_copula(pilot_rest_data$x_train,pilot_rest_data$y_train,n0_train,n1_train,n0_test,n1_test)}

        }else if(test_from_true==1){

          train_test_data = split_data(data$x_data, data$y_data, n_train=n_train, n_test=n_test)

          if(method == "pca2_mvnorm"){result = pilot_train_pca2(train_test_data$x_train,train_test_data$y_train,train_test_data$x_test)}
          if(method == "gaussian_copula"){result = pilot_train_pca2(train_test_data$x_train,train_test_data$y_train,train_test_data$x_test)}
        }


        p.LR[L,] = result$p.LR

        p.RF[L,] = result$p.RF

        p.svm[L,] = result$p.svm

        p.xgb[L,] = result$p.xgb

        cat(count,L,"\n")

      }


      for (k in 1: Loop)
      {
        index0_test=which(test_y==0)
        index1_test=which(test_y==1)

        score.LR=p.LR[k,]
        roc.LR<-roc.curve(scores.class0 =score.LR[index1_test],scores.class1=score.LR[index0_test],curve =FALSE)

        LR.auc[count,k]=roc.LR$auc


        score.RF=p.RF[k,]
        roc.RF<-roc.curve(scores.class0 =score.RF[index1_test],scores.class1=score.RF[index0_test],curve =FALSE)

        RF.auc[count,k]=roc.RF$auc

        score.svm=p.svm[k,]
        roc.svm<-roc.curve(scores.class0 =score.svm[index1_test],scores.class1=score.svm[index0_test],curve =FALSE)

        svm.auc[count,k]=roc.svm$auc

        score.xgb=p.xgb[k,]
        roc.xgb<-roc.curve(scores.class0 =score.xgb[index1_test],scores.class1=score.xgb[index0_test],curve =FALSE)

        xgb.auc[count,k]=roc.xgb$auc

      }
      count = count + 1
    }
  }

  result = list(LR_AUC = LR.auc, RF_AUC = RF.auc, SVM_AUC = svm.auc, XGB_AUC = xgb.auc)
  return(result)
}
