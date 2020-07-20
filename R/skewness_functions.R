#All the following functions calculate sigma2^1.5 * component
#(later we will multipy by sigma2^-1.5 to get the right value)
#this is done because we need W for the SE calculations

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
  w2 <- with(ss, sum(p_g * (mu_g^2 - mu2_g)))
  W <- w1 + mu*w2
  return(W)
}

var_between.skewness <- function(ss, W){
  #extract sample level statistics
  mu <- ss$mu1[1]
  sigma <- sqrt(ss$sigma2[1])
  sum_p_i2 <- ss$sum_p_i2[1]
  tmp <- with(ss, 3*p_g*mu_g^2 + p_g*6*mu^2 -3*(p_g*sum(p_g*mu_g^2) + 2*mu*mu_g*p_g))
  A_g <- with(ss, 3*mu*W*p_g / sigma^5 + tmp / sigma^3)
  B_g <- with(ss, -1.5*p_g*W / sigma^5)
  tmp <- with(ss, mu_g^3 + mu_g*6*mu^2  - 3*(mu_g*sum(p_g*mu_g^2) + mu*mu_g^2))
  C_g <- with(ss, -1.5*W*(mu2_g - 2*mu*mu_g) / sigma^5 + tmp / sigma^3)
  res <- with(ss, sum(var_mu_g * A_g^2 + var_mu2_g * B_g^2))
  res <- res + calc_p_term(p = ss$p_g, const = C_g, sum_p_i2)
  res <- res + 2* sum (A_g * B_g * ss$cov_mu_g_mu2_g)
  return(res)
}


