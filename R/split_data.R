#' @title Split Data
#'
#' @param x_data x data you want to split
#' @param y_data y data you want to split: it must be binary data and the class label should be "0" and "1"
#' @param n0_train
#' Int, represents the absolute number of class 0 training data.
#' @param n1_train
#' Int, represents the absolute number of class 1 training data.
#' @param n0_test
#' Int, represents the absolute number of class 0 test data.
#' If NULL, equals to n0_all minus n0_train.
#' @param n1_test
#' Int, represents the absolute number of class 1 test data.
#' If NULL, equals to n1_all minus n1_train.
#' @param seed
#' Controls the shuffling applied to the data before applying the split.
#' Pass an int as the seed for reproducible output across multiple function calls.
#'
#' @return the split data
#' @export
#'
#' @examples
#'
#' data = generate_data(n01_all=c(800,800))
#' data_split1 = split_data(data$x_data, data$y_data, n0_train=500, n1_train=500, seed=1)
#' data_split2 = split_data(data$x_data, data$y_data, n0_train=500, n1_train=500, n0_test=200, n1_test=200, seed=1)
#' data_split3 = split_data(data$x_data, data$y_data, n0_train=500, n1_train=500, n0_test=200, n1_test=NULL, seed=1)
#'

split_data <- function(x_data, y_data, n0_train, n1_train, n0_test=NULL, n1_test=NULL, seed=NULL)
{

  id0 <- which(y_data==0)
  id1 <- which(y_data==1)

  n0_all = length(id0)
  n1_all = length(id1)
  # check n_all ?= length(y_data)

  if(!is.null(seed)){
    set.seed(seed)
  }

  if(n0_train > n0_all || n1_train > n1_all){
    stop('split data error: generated or input data are not sufficient to split!')
  }

  id0_train <- sample(id0,n0_train)
  id1_train <- sample(id1,n1_train)
  id_train <- c(id0_train,id1_train)

  x_train <- as.matrix(x_data[id_train,])
  y_train <- as.matrix(y_data[id_train])

  id0_remain = setdiff(id0,id0_train)
  id1_remain = setdiff(id1,id1_train)
  id_remain <- c(id0_remain,id1_remain)

  if((is.null(n0_test)&&is.null(n1_test))||(((n1_train+n1_test)==n1_all)&&(n0_train+n0_test)==n0_all)){
    x_test <- as.matrix(x_data[id_remain,])
    y_test <- as.matrix(y_data[id_remain])
    result = list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test)
    return(result)
  }
  else{
    n0_remain = length(id0_remain)
    n1_remain = length(id1_remain)
    if(!is.null(n0_test)&&is.null(n1_test)){
      if((n0_train+n0_test) > n0_all){
        stop('split data error: generated or input data are not sufficient to split!')
      }
      n1_test = n1_all - n1_train
    }else if(is.null(n0_test)&&!is.null(n1_test)){
      if((n1_train+n1_test) > n1_all){
        stop('split data error: generated or input data are not sufficient to split!')
      }
      n0_test = n0_all - n0_train
    }
    else{
      if(((n0_train+n0_test) > n0_all) || ((n1_train+n1_test) > n1_all)){
        stop('split data error: generated or input data are not sufficient to split!')
      }
    }
    id0_test <- sample(id0_remain,n0_test)
    id1_test <- sample(id1_remain,n1_test)
    id_test <- c(id0_test,id1_test)
    x_test <- as.matrix(x_data[id_test,])
    y_test <- as.matrix(y_data[id_test])

    id0_remain2 = setdiff(id0_remain,id0_test)
    id1_remain2 = setdiff(id1_remain,id1_test)
    id_remain2 <- c(id0_remain2,id1_remain2)

    x_remain <- as.matrix(x_data[id_remain2,])
    y_remain <- as.matrix(y_data[id_remain2])

    result = list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test, x_remain=x_remain, y_remain=y_remain)
    return(result)
  }

}
