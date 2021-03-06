% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_data.R
\name{split_data}
\alias{split_data}
\title{Split Data}
\usage{
split_data(
  x_data,
  y_data,
  n0_train,
  n1_train,
  n0_test = NULL,
  n1_test = NULL,
  seed = NULL
)
}
\arguments{
\item{x_data}{x data you want to split}

\item{y_data}{y data you want to split: it must be binary data and the class label should be "0" and "1"}

\item{n0_train}{Int, represents the absolute number of class 0 training data.}

\item{n1_train}{Int, represents the absolute number of class 1 training data.}

\item{n0_test}{Int, represents the absolute number of class 0 test data.
If NULL, equals to n0_all minus n0_train.}

\item{n1_test}{Int, represents the absolute number of class 1 test data.
If NULL, equals to n1_all minus n1_train.}

\item{seed}{Controls the shuffling applied to the data before applying the split.
Pass an int as the seed for reproducible output across multiple function calls.}
}
\value{
the split data
}
\description{
Split Data
}
\examples{

data = generate_data(n01_all = c(800, 800))
data_split1 = split_data(data$x_data, data$y_data, n0_train = 500, n1_train = 500, 
    seed = 1)
data_split2 = split_data(data$x_data, data$y_data, n0_train = 500, n1_train = 500, 
    n0_test = 200, n1_test = 200, seed = 1)
data_split3 = split_data(data$x_data, data$y_data, n0_train = 500, n1_train = 500, 
    n0_test = 200, n1_test = NULL, seed = 1)

}
