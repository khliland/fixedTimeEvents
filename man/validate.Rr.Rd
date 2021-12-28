\name{validate.Rr}
\alias{validate.Rr}
\title{
Validation of Liland distribution parameters.
}
\description{
Checks if parameters conform to R >= 2, r >= 2 and r <= R.
}
\usage{
validate.Rr(R, r)
}
\arguments{
  \item{R}{
    number of Bernoulli trials.
}
  \item{r}{
    number of successes.
}
}
\value{
No return, only testing.
}
\references{
Liland, KH & Snipen, L, FixedTimeEvents: An R package for the distribution of
distances between discrete events in fixed time, SoftwareX 5 (2016).
}
\author{
Kristian Hovde Liland
}

\seealso{
\code{\link{dLiland}}
}
\examples{
validate.Rr(20,10)
\dontrun{
# r>R results in an error.
  validate.Rr(20,30)
}
}