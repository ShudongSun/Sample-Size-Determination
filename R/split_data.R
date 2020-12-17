#' @title Split Data
#'
#' @param x_data x data you want to split
#' @param y_data y data you want to split: it must be binary data and the class label should be "0" and "1"
#' @param n_train
#' If float, should be between 0.0 and 1.0 and represent the proportion of the dataset to include in the train split.
#' If int, represents the absolute number of train samples (It will extract half from class 0 and half from class 1).
#' If None, the value is automatically set to the complement of the test size.
#' Its default value is set to 0.7
#' @param n_test
#' If float, should be between 0.0 and 1.0 and represent the proportion of the dataset to include in the train split.
#' If int, represents the absolute number of train samples (It will extract half from class 0 and half from class 1).
#' If NULL, the value is automatically set to the complement of the training size.
#' @param seed
#' Controls the shuffling applied to the data before applying the split.
#' Pass an int as the seed for reproducible output across multiple function calls.
#'
#' @return The split data
#' @export
#'
#' @examples
#'
#' data = generate_data()
#' data_split1 = split_data(data$x_data, data$y_data, n_train=30, seed=1)
#' data_split2 = split_data(data$x_data, data$y_data, n_train=1000, n_test=500, seed=1)
#' data_split3 = split_data(data$x_data, data$y_data, n_train=0.6, n_test=0.4, seed=1)
split_data <- function(x_data, y_data, n_train=0.7, n_test=NULL, seed=NULL)
{

  id0 <- which(y_data==0)
  id1 <- which(y_data==1)

  n0_all = length(id0)
  n1_all = length(id1)
  # check n_all ?= length(y_data)
  if(n_train>=1){
    n0_train = round(n_train / 2)
    n1_train = n_train - n0_train
  }
  else{
    n0_train = round(n0_all * n_train)
    n1_train = round(n1_all * n_train)
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  id0_train <- sample(id0,n0_train)
  id1_train <- sample(id1,n1_train)
  id_train <- c(id0_train,id1_train)

  x_train <- as.matrix(x_data[id_train,])
  y_train <- as.matrix(y_data[id_train])

  id0_remain = setdiff(id0,id0_train)
  id1_remain = setdiff(id1,id1_train)
  id_remain <- c(id0_remain,id1_remain)

  if(is.null(n_test)||(n_train+n_test==1)||((n_train+n_test)==(n0_all+n1_all))){
    x_test <- as.matrix(x_data[id_remain,])
    y_test <- as.matrix(y_data[id_remain])
    result = list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test)
    return(result)
  }
  else{
    n0_remain = length(id0_remain)
    n1_remain = length(id1_remain)
    if(n_test>=1){
      n0_test = round(n_test / 2)
      n1_test = n_test - n0_test
    }
    else{
      n0_test = round(n0_all * n_test)
      n1_test = round(n1_all * n_test)
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
