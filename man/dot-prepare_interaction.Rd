% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_interactions.R
\name{.prepare_interaction}
\alias{.prepare_interaction}
\title{.prepare_interaction}
\usage{
.prepare_interaction(df, intTerms, nlvl, ...)
}
\arguments{
\item{df}{A data.frame that contains the interaction terms to be prepared}

\item{intTerms}{A list of names that identify the columns in df to be prepared}

\item{nlvl}{Max number of levels allowed in an interaction term before its binned}

\item{...}{Additional arguments to be passed to the binnarise function}
}
\description{
Internal function that prepares terms for interaction creation
}
\examples{

}
\keyword{interaction}
\keyword{prepare}
