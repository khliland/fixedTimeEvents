# Mean and variance
Liland <- function(R,r){
  x <- c((R+1)/(r+1), r*(R+1)*(R-r)/((r+1)^2*(r+2)))
  names(x) <- c("Mean","Variance")
  x
}
