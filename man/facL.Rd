\name{facL}
\alias{facL}
\title{
Approximated logarithm of factorials
}
\description{
Stirling's 2nd order approximation of the logarithm of a factorial.
}
\usage{
facL(n)
}
\arguments{
  \item{n}{
    vector of integers for which to compute the logarithmic factorial.
}
}
\value{
The logarithm of the factorial.
}
\author{
Kristian Hovde Liland
}

\references{
Liland, KH & Snipen, L, FixedTimeEvents: An R package for the distribution of
distances between discrete events in fixed time, SoftwareX 5 (2016).
}

\seealso{
\code{\link{dLiland}}, \code{\link{Liland}}, \code{\link{Liland.test}}, \code{\link{simLiland}}
}
\examples{
# Some values of the logarithm of factorials.
facL( c(2,10,100,1000) )
log( factorial( c(2,10,100,1000) ) )

# Fraction of two factorials
exp( facL(200)-facL(180) )
factorial(200)/factorial(180)
}