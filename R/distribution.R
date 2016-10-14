##
## The Liland distribution
##
## The distribution of distances between discrete events in fixed time/space
##

# Probability mass function
dLiland <- function(x, R, r, warn=FALSE){
  validate.Rr(R,r)       # Check if R and r are usable
  x[x>R+1-r | x<1] <- NA # Remove impossible values
  denom <- choose(R,r)
  if(denom<10^20){
    x <- choose(R-x,r-1)/denom
  } else {
    if(warn)
      warning('Binomial coefficient too large, approximating')
    x <- exp(facL(R-x)-facL(r-1)-facL(R-x-r+1) - facL(R)+facL(r)+facL(R-r))
  }
  NA2NaN( x )
}

# Cummulative distribution function
pLiland <- function(q, R, r, lower.tail=TRUE, warn=FALSE){
  validate.Rr(R,r)       # Check if R and r are usable
  q[q>R+1-r | q<1] <- NA # Remove impossible values
  denom <- choose(R,r)
  if(lower.tail){
    if(denom<10^20){
      x <- 1-choose(R-q,r)/denom
    } else {
      if(warn)
        warning('Binomial coefficient too large, approximating')
      x <- rep(1,length(q))
      x[q!=R-r+1] <- 1-exp(facL(R-q[q!=R-r+1])-facL(r)-facL(R-q[q!=R-r+1]-r) - facL(R)+facL(r)+facL(R-r))
    }
  } else {
    if(denom<10^20){
      x <- choose(R-q,r)/denom
    } else {
      if(warn)
        warning('Binomial coefficient too large, approximating')
      x <- rep(1,length(q))
      x[q!=R-r+1] <- exp(facL(R-q[q!=R-r+1])-facL(r)-facL(R-q[q!=R-r+1]-r) - facL(R)+facL(r)+facL(R-r))
    }
  }
  NA2NaN( x )
}

# Quantile function
# qLiland <- function(p, R, r){
#   validate.Rr(R,r)   # Check if R and r are usable
#   p[p<0 | p>1] <- NA # Remove impossible values
#   
#   ppLiland <- function(k,p,R,r){pLiland(k,R,r)-p}
#   
#   lp <- length(p)
#   x  <- numeric(lp)
#   ps <- sort(p)
#   j  <- 1
#   for(i in 1:length(ps)){
#     found <- FALSE
#     while(!found){
#       p_new <- ppLiland(j,ps[i],R,r)
#       if(p_new >= 0){
#         found <- TRUE
#         x[i] <- j
#       } else {
#         pp <- p_new
#         j  <- j+1
#       }
#     }
#   }
#   x <- x[match(p,ps)]
#   x
# }
qLiland <- function(p, R, r){
  validate.Rr(R,r)   # Check if R and r are usable
  p[p<0 | p>1] <- NA # Remove impossible values
  lp <- length(p)
  x  <- numeric(lp)
  ps <- sort(p)
  for(i in 1:length(ps)){
    found <- FALSE
    j  <- 1
    while(!found){
      ps <- pLiland( ((j-1)*10^6+1) : min(j*10^6,R-r), R, r)
      xt <- match(FALSE, ps <= p[i])
      if(length(xt)>0){
        found <- TRUE
        x[i] <- xt + (j-1)*10^6
      }
      j <- j+1
    }
  }
  x
}

# Random number
rLiland <- function(n, R, r){
  validate.Rr(R,r) # Check if R and r are usable
  if(n<0){         # Negative n
    stop("invalid arguments") }
  
  distr <- pLiland(1:(R-r+1),R,r)
  k <- runif(n)
  vapply(k, function(i) which.max(i<=distr), 0L)
}

# Random set of numbers
rrLiland <- function(n,R,r){
  validate.Rr(R,r) # Check if R and r are usable
  if(n<0){         # Negative n
    stop("invalid arguments") }
  D <- numeric(n)
  for(i in 1:n){
    D[i] <- mean(diff(sort(sample(R,r))))
  }
  D
}
