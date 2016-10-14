\name{simLiland}
\alias{simLiland}
\alias{simLiland2}
\alias{simLilandMu}
\title{
Simulations for the Liland distribution.
}
\description{
Three different simulations are provided for the Liland distribution. These include
sampling repeatedly from a given Liland distribution, sampling from the Bernoulli
distribution and summarizing, and sampling random mean Liland numbers.
}
\usage{
simLiland(S, R, r)
simLiland2(S, R, r)
simLilandMu(S, R, r)
}
\arguments{
  \item{S}{
    number of samples.
}
  \item{R}{
    number of trials or denominator of Bernoulli probability.
}
  \item{r}{
    number of successes or numerator of Bernoulli probablity.
}
}
\value{
\code{simLiland} returns a vector of simulated Liland probabilities.
\code{simLiland2} returns a list of sampled counts (\code{res}),
summary of counts (\code{counts}) and order of counts (\code{ms}).
\code{simLilandMu} returns a vector of simulated mean Liland numbers.
}
\references{
Liland, KH & Snipen, L, FixedTimeEvents: An R package for the distribution of
distances between discrete events in fixed time, SoftwareX, in press.
}
\author{
Kristian Hovde Liland
}

\seealso{
\code{\link{dLiland}}, \code{\link{Liland}}, \code{\link{Liland.test}}
}
\examples{
simLiland(1000,20,10)
sl <- simLiland2(1000,20,10)
sl$counts[sl$ms]/1000
plot(density(simLilandMu(1000,20,10)))
}
