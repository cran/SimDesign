% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{colVars}
\alias{colVars}
\alias{colSDs}
\title{Form Column Standard Deviation and Variances}
\usage{
colVars(x, na.rm = FALSE, unname = FALSE)

colSDs(x, na.rm = FALSE, unname = FALSE)
}
\arguments{
\item{x}{an array of two dimensions containing numeric, complex, integer or logical values,
or a numeric data frame}

\item{na.rm}{logical; remove missing values in each respective column?}

\item{unname}{logical; apply \code{\link{unname}} to the results to remove any variable
names?}
}
\description{
Form column standard deviation and variances for numeric arrays (or data frames).
}
\examples{

results <- matrix(rnorm(100), ncol=4)
colnames(results) <- paste0('stat', 1:4)

colVars(results)
colSDs(results)

results[1,1] <- NA
colSDs(results)
colSDs(results, na.rm=TRUE)
colSDs(results, na.rm=TRUE, unname=TRUE)

}
\seealso{
\code{\link{colMeans}}
}
\author{
Phil Chalmers \email{rphilip.chalmers@gmail.com}
}
