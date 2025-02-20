% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getdata.R
\name{getdata}
\alias{getdata}
\title{Gets relevant data from a specified snapshot.}
\usage{
getdata(local = F, snapshot = NULL)
}
\arguments{
\item{local}{TRUE or FALSE, whether to use a local file or pull from GitHub. Can be used if you don't have an internet connection.}

\item{snapshot}{string specifying which snapshot to use, e.g., "BioDIGS_20250206.csv"}
}
\value{
an uncleaned \code{data.frame}
}
\description{
Gets relevant data from a specified snapshot.
}
\examples{
getdata()
getdata(snapshot = "BioDIGS_20250206.csv")
}
\keyword{internal}
