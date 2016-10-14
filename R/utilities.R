##
## Utility functions
##

# Validate R and r
validate.Rr <- function(R,r){
  if(r<2 || R<2)
    stop("R and r must both be 2 or larger")
  if(r>R)
    stop("r can not be larger than R")
}

# Change NAs to NaNs
NA2NaN <- function(k){
  if(any(is.na(k)))
    warning("NaN(s) produced", call.=FALSE)
  k[is.na(k)] <- NaN
  k
}

# Stirling's approximation to the log of a factorial
facL <- function(n){
  x <- n*log(n) + log(n*sqrt(2*pi/n)) - n + log(1 + 1/(12*n))
  x[n==0] <- 0
  x
}
