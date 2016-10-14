\name{NA2NaN}
\alias{NA2NaN}
\title{
Translation of values from NA (not available) to NaN (not a number)
}
\description{
Exchanges all occurrences of NA in a vector with NaN. A warning is issued when NAs or NaNs are found.
}
\usage{
NA2NaN(k)
}
\arguments{
  \item{k}{
    numerical vector possibly containig NAs.
}
}
\value{
Returns a vector where possible NAs have been changed to NaNs.
}
\author{
Kristian Hovde Liland
}

\seealso{
\code{\link{dLiland}}, \code{\link{Liland}}, \code{\link{Liland.test}}, \code{\link{simLiland}}
}
\examples{
NA2NaN( c(0, 1, NA, NaN))
NA2NaN( c(0, 1, 2, NaN))
NA2NaN( c(0, 1, NA, 100))
NA2NaN( c(0, 1, 2, 100))
}