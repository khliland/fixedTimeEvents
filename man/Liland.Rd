\name{Liland}
\alias{Liland}
\title{
Properties of the Liland distribution
}
\description{
Calculates the mean and variance of the Liland distribution according to the number of trials and successes.
}
\usage{
Liland(R, r)
}
\arguments{
  \item{R}{
    number of trials.
}
  \item{r}{
    number of successes.
}
}
\value{
Returns a named vector containing the mean and variance of the Liland distribution.
}
\references{
Liland, KH & Snipen, L, FixedTimeEvents: An R package for the distribution of
distances between discrete events in fixed time, SoftwareX, in press.
}
\author{
Kristian Hovde Liland
}

\seealso{
\code{\link{dLiland}}, \code{\link{Liland.test}}, \code{\link{simLiland}}
}
\examples{
Liland(R = 1949, r = 162)
}