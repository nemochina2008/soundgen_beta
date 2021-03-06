% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSM.R
\name{getNovelty}
\alias{getNovelty}
\title{SSM novelty}
\usage{
getNovelty(ssm, kernelSize, kernelSD)
}
\arguments{
\item{ssm}{self-similarity matrix, as produced by \code{\link{selfsim}}}

\item{kernelSize}{the size of gaussian kernel (points)}

\item{kernelSD}{the SD of gaussian kernel}
}
\value{
Returns a numeric vector of length \code{nrow(ssm)}
}
\description{
Internal soundgen function.
}
\details{
Calculates novelty in a self-similarity matrix. Called by \code{\link{ssm}}.
}
\keyword{internal}
