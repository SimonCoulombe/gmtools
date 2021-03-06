% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgboost_binnariser.R
\name{xgb_band}
\alias{xgb_band}
\title{xgb_band}
\usage{
xgb_band(dtrain, x, y, w = NULL, max_levels, xgbParams, ...)
}
\arguments{
\item{dtrain}{A data.frame that contains the training data}

\item{x}{A list of column names that identify the explanatory features}

\item{y}{A column name that identifies that target variable}

\item{w}{A column name that contains the name of the weight column. Defaults to NULL}

\item{max_levels}{Whats the maximum number of levels the model should create}

\item{xgbParams}{A named list containing the XGBoost Learning Parameters}

\item{...}{Additional Arguments to be passed to train_xgboost}
}
\description{
A wrapper function that automates the xgboost training process
}
\examples{

}
\keyword{banding}
\keyword{xgboost}
