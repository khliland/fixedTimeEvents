\name{Liland.test}
\alias{Liland.test}
\alias{print.Ltest}
\alias{Liland.crit}
\alias{Liland.pow}
\alias{summary.Ltest}
\title{
A test for over represented short distances in the Liland distribution.
}
\description{
A binomial test is performed using probabilites from the Liland distribution to check
if the number of distances shorter to or equal to \code{xlim} are significantly
higher than the expected value. Critical value and power are supplied as separate functions.
}
\usage{
Liland.test(y, xlim, R, r)
\method{print}{Ltest}(x, \dots)
\method{summary}{Ltest}(object, \dots)
Liland.crit(xlim, R, r, alpha = 0.05)
Liland.pow(xlim, R, r, y = 1:(r-1), alpha = 0.05)
}
\arguments{
  \item{y}{
  The number of observed short distances.
}
  \item{xlim}{
  The maximum distance that is seen as short.
}
  \item{R}{
  The number of trials.
}
  \item{r}{
  The number of successes.
}
  \item{alpha}{
  Significance level.
}
  \item{x}{
  The object to printed.
}
  \item{object}{
  The object to be summarized.
}
  \item{\dots}{
  Additional arguments for print and summary (not used).
}
}
\value{
\code{Liland.test} returns a named vector of P-values with class \code{Ltest}. The other methods only print.
}

\references{
Liland, KH & Snipen, L, FixedTimeEvents: An R package for the distribution of
distances between discrete events in fixed time, SoftwareX 5 (2016).
}

\seealso{
\code{\link{dLiland}}, \code{\link{Liland}}, \code{\link{simLiland}}
}
\examples{
Lt <- Liland.test(12,1,1949,162)
print(Lt)
summary(Lt)

# Critical value
Liland.crit(1, 1949, 162) 

# Power
plot(Liland.pow(1,1949,161, alpha = 0.05), type = 'l', xlab = '#(x<2)', ylab = 'power')
}