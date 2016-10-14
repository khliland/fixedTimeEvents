##
## Simulation
##

# Draw r from R without replacement S times. Report proportions
simLiland <- function(S, R, r){
  res <- numeric(R-r+1)
  MM <- 1:R
  for(i in 1:S){
    tmp <- diff(sort(sample(MM,r,FALSE)))
    for(j in 1:(r-1))
      res[tmp[j]] <- res[tmp[j]]+1
  }
  res/(S*(r-1))
}

# Draw r from R without replacement S times. Report means
simLilandMu <- function(S, R, r){
  res <- numeric(S)
  MM <- 1:R
  for(i in 1:S){
    res[i] <- mean(diff(sort(sample(MM,r,FALSE))))
  }
  res
}

# Draw uniformly R times with probability r/R. Repeat S times and report.
simLiland2 <- function(S, R, r){
  ms <- numeric()
  res <- list(res=list(),counts=numeric())
  s <- 1
  while(s <= S){
    tmp <- numeric(R)
    k <- 1
    n <- 0
    found <- FALSE
    while(!found){
      t1 <- rbinom(1,1,r/R)
      if(t1==1){
        found <- TRUE
      } else {
        k <- k+1
      }
    }
    for(j in (k+1):R){
      t2 <- rbinom(1,1,r/R)
      if(t2==1){
        tmp[j-k] <- tmp[j-k]+1
        k <- j
        n <- n+1
      }
    }
    if(n>0 && n<R){
      s <- s+1
      mn <- match(n,ms)
      if(is.na(mn)){
        ms <- c(ms,n)
        res$res[[n]] <- tmp[1:(R-n)]
        res$counts[n] <- 1
      } else {
        res$res[[n]] <- res$res[[n]] + tmp[1:(R-n)]
        res$counts[n] <- res$counts[n]+1
      }
    }
  }
  res$ms <- ms
  res
}
