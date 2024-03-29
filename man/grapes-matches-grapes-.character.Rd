% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec.R
\name{\%matches\%.character}
\alias{\%matches\%.character}
\title{Check Whether Character matches y}
\usage{
\method{\%matches\%}{character}(x, y, ...)
}
\arguments{
\item{x}{character}

\item{y}{object}

\item{...}{passed arguments}
}
\description{
Checks whether character matches y, treating x as filepath.
}
\examples{
data(drug)
file <- tempfile()
spec <- specification(drug, tol = 3)
library(csv)
as.csv(drug, file)
file \%matches\% spec
}
\seealso{
Other matches: 
\code{\link{\%matches\%.data.frame}()},
\code{\link{\%matches\%.spec}()},
\code{\link{\%matches\%}()}
}
\concept{matches}
