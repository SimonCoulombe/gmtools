% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_xgboost.R
\name{train_xgboost}
\alias{train_xgboost}
\title{train_xgboost}
\usage{
train_xgboost(dtrain, dvalid = NULL, x, y, w = NULL, base_margin = NULL,
  xgbParams, nrounds = 5000, early_stopping_rounds = 5, nfold = 5,
  folds = NULL, verbose = TRUE, seed = 1921, ...)
}
\arguments{
\item{dtrain}{A data.frame that contains the training data}

\item{dvalid}{A data.frame that contains the validation data. If NULL then the function uses cross validation. Defaults to NULL.}

\item{x}{A list of column names that identify the explanatory features}

\item{y}{A column name that identifies that target variable}

\item{w}{A column name that contains the name of the weight column. Defaults to NULL}

\item{xgbParams}{A named list containing the XGBoost Learning Parameters}

\item{nrounds}{The maximum number of iterations}

\item{early_stopping_rounds}{If performance doesnt improve for this many rounds then stop training}

\item{nfold}{How many folds to use if doing cross validation}

\item{folds}{A list of pre defined fold indicies (test indicies) - see main xgboost docs for more details.}

\item{verbose}{Print run time messages}

\item{seed}{An integer which will be used as the random seed. Defaults to 1921}

\item{...}{Additional Arguments to be passed to xgb.cv and/or xgb.train}
}
\description{
A wrapper function that automates the xgboost training process
}
\examples{

}
\keyword{train}
\keyword{xgboost}
