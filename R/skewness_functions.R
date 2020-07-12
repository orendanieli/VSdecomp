#All the following functions calculate sigma2^1.5 * component
#(later we will multipy by sigma2^-1.5 to get the right value)
#this is done because we need W for the SE calculations

#currently these function dont work properly
#they not pass the theoretical test
between.skewness <- function(ss){
  #extract sample level statistics
  mu <- ss$mu1[1]
  W <- with(ss, sum(p_g * mu_g^3) - 3*mu*sum(p_g * mu_g^2) +2*mu^3)
  return(W)
}

within.skewness <- function(ss){
  W <- with(ss, sum(p_g * (mu3_g - 3*mu_g*sigma2_g - mu_g^3)))
  return(W)
}

cov.skewness <- function(ss){
  #extract sample level statistics
  mu <- ss$mu1[1]
  w1 <- with(ss, sum(p_g * (mu2_g*mu_g - mu_g^3)))
  w2 <- with(ss, sum(p_g * (mu_g^3 - mu3_g)))
  W <- w1 + mu*w2
  return(W)
}




