\name{rrLiland}
\alias{rrLiland}
\title{
Random Bernoulli trials for Liland distributed mean numbers.
}
\description{
r successes are drawn from R trials. This is repeated n times to produce
a random vector of mean Liland distributed numbers.
}
\usage{
rrLiland(n, R, r)
}
\arguments{
  \item{n}{
    number of repeated samples.
}
  \item{R}{
    number of Bernoulli trials.
}
  \item{r}{
    number of successes per sample.
}
}
\value{
Vector of mean distance between successful events.
}
\references{
Liland, KH & Snipen, L, FixedTimeEvents: An R package for the distribution of
distances between discrete events in fixed time, SoftwareX, in press.
}
\author{
Kristian Hovde Liland
}

\seealso{
\code{\link{dLiland}}, \code{\link{Liland}}, \code{\link{Liland.test}}, \code{\link{simLiland}}
}
\examples{
mdist <- rrLiland(1000, 25, 7)
plot(density(mdist))
}
