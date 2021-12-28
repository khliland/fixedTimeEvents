\name{dLiland}
\alias{fixedTimeEvents}
\alias{dLiland}
\alias{pLiland}
\alias{qLiland}
\alias{rLiland}

\title{
The distribution of distances between discrete events in fixed time/space (the Liland distribution)
}
\description{
Density, distribution function, quantile function and random generation 
for the Liland distribution with \code{R} trials and \code{r} successes.
}
\usage{
dLiland(x, R, r, warn = FALSE)
pLiland(q, R, r, lower.tail = TRUE, warn = FALSE)
qLiland(p, R, r)
rLiland(n, R, r)
}
\arguments{
  \item{x, q}{
    vector of quantiles.
}
  \item{p}{
    vector of probabilities.
}
  \item{n}{
    number of observations.
}
  \item{R}{
    number of trials.
}
  \item{r}{
    number of successes.
}
  \item{warn}{
    logical indicating if a warning should be issued if approximation is used.
}
  \item{lower.tail}{
    logical indicating if the lower tail of the distribution should be summed.
}
}
\details{
The Liland distribution has probability mass
\deqn{
  f(X=x;R,r) = 
  \frac{{R-x \choose r-1}}{{R \choose r}}}{
  f(X=x;R,r) = binomial(R-x|r-1)/binomial(R|r)
  }
where \eqn{x} is the distance between consecutive successes, \eqn{R}
is the number of trials and \eqn{r} is the number of successes.
}
\value{
\code{dLiland} gives the probability mass, \code{pLiland} gives the distribution
function, \code{qLiland} gives the quantile function, and \code{rLiland} generates
random Liland values.
}
\references{
Liland, KH & Snipen, L, FixedTimeEvents: An R package for the distribution of
distances between discrete events in fixed time, SoftwareX 5 (2016).
}
\author{
Kristian Hovde Liland
}

\seealso{
\code{\link{Liland}}, \code{\link{Liland.test}}, \code{\link{simLiland}}
}
\examples{
dLiland(19, R = 1949, r = 162)
pLiland(19, R = 1949, r = 162)
qLiland(0.5, R = 1949, r = 162)
plot( pLiland(1:100, R = 1949, r = 162) )

## QQ-plot of Liland distribution and random Liland values
R <- 2000
r <- 120
n <- 1000
samp <- rLiland(n,R,r)
theo <- qLiland(ppoints(n),R,r)
qqplot(theo,samp,
       xlab='F(x;2000,120)', ylab='Sample (1000)', axes=FALSE)
axis(1,at=c(0,40,80,120))
axis(2,at=c(0,40,80,120))
box()
qqline(samp, distribution = function(p)qLiland(p,R=2000,r=120), col='gray',lty=2)
}
