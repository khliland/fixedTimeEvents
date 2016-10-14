Liland.test <- function(y, xlim, R, r){
  p <- pLiland(xlim,R,r)
  L <- pbinom(y, r-1, p, lower.tail = FALSE)
  attr(L,'mu') <- (r-1)*p
  attr(L,'p')  <- p
  attr(L,'inputs') <- c(xlim,R,r)
  class(L) <- 'Ltest'
  names(L) <- y
  L
}
print.Ltest <- function(x, ...){
  s <- ifelse(length(x)==1,'','s')
  cat('P-value',s,' for H0: y > E(Y)\n', sep="")
  nam <- names(x); attributes(x) <- NULL
  names(x) <- nam
  print.default(x)
}
summary.Ltest <- function(object, ...){
  inputs <- attr(object,'inputs')
  cat('R = ',inputs[2],', r = ',inputs[3],', xlim = ',inputs[1], '\n', sep="")
  cat('E(Y) = ', attr(object,'mu'), '\n\n', sep="")
  print(object)
}

# Critical value
Liland.crit <- function(xlim, R, r, alpha = 0.05){
  p <- pLiland(xlim,R,r)
  crit <- qbinom(1-alpha, r-1, p)
  crit
}

# Power
Liland.pow <- function(xlim, R, r, y = 1:(r-1), alpha = 0.05){
  crit <- Liland.crit(xlim,R,r)
  pow  <- 1-pbinom(crit, r-1, y/(r-1))
  pow
}
