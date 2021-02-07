#Calculates sufficient statistics for the variance decomposition
 
suf_stat <- function(y, x, wgt = rep(1, length(y)), moment = "skewness"){
  d <- data.frame(y, x, wgt)
  d$x <- as.factor(d$x)
  splitted_d <- split(d, d$x)
  n_groups <- length(splitted_d)
  ss <- data.frame(mu_g = rep(NA, n_groups),
                   mu2_g = rep(NA, n_groups),
                   mu3_g = rep(NA, n_groups),
                   mu4_g = rep(NA, n_groups),
                   mu5_g = rep(NA, n_groups),
                   mu6_g = rep(NA, n_groups))
  for(i in 1:6){
    ss[,i] <- unlist(lapply(splitted_d, function(g){wtd.mean(g$y^i, g$wgt)}))
  }
  sum_w <- sum(wgt)
  ss$p_g <- unlist(lapply(splitted_d, function(g){sum(g$wgt) / sum_w}))
  ss$sum_p_i2_g <- unlist(lapply(splitted_d, function(g){sum(g$wgt^2) / sum(g$wgt)^2}))
  ss$sigma2_g <- ss$mu2_g - ss$mu_g^2
  ss$var_mu_g <- ss$sigma2_g * ss$sum_p_i2_g
  ss$var_sigma2_g <- with(ss, sum_p_i2_g * (mu4_g - mu2_g^2 + 4*sigma2_g*mu_g^2 - 4*mu_g*(mu3_g - mu_g*mu2_g)))
  #add sample level statistics
  ss$sum_p_i2 <- sum(wgt^2) / sum_w^2 
  ss$mu1 <- with(ss, sum(mu_g * p_g))
  #more statistics needed for skewness calculations
  if(moment == "skewness"){
    ss$sigma2 <- wtd_var(y, wgt)
    ss$var_mu2_g <- with(ss, sum_p_i2_g * (mu4_g - mu2_g^2))
    ss$var_mu3_g <- with(ss, sum_p_i2_g * (mu6_g - mu3_g^2))
    ss$cov_mu_g_mu2_g <- with(ss, (mu3_g - mu_g*mu2_g) * sum_p_i2_g)
    ss$cov_mu_g_mu3_g <- with(ss, (mu4_g - mu_g*mu3_g) * sum_p_i2_g)
    ss$cov_mu2_g_mu3_g <- with(ss, (mu5_g - mu2_g*mu3_g) * sum_p_i2_g)
  }
  return(ss)
}

within.var <- function(ss){
  return(wtd.mean(ss$sigma2_g, ss$p_g))
}

between.var <- function(ss){
  wtd.mean(ss$mu_g^2, ss$p_g) - wtd.mean(ss$mu_g, ss$p_g)^2
}

#this function calculates the part of the variance that depends on p_g
#sum_p_i2 is the equivalent of 1/N 
#(in the special case of V i: w_i = 1, sum_p_i2 = 1/N)
calc_p_term <- function(p, const, sum_p_i2){
  const_mat <- const %*% t(const)
  p_mat <- p %*% t(p)
  diag(p_mat) <- p * (1 - p)
  #multiply cov by -1
  p_mat <- -1*p_mat
  diag(p_mat) <- -1*diag(p_mat)
  res <- sum(p_mat * const_mat) * sum_p_i2 
  return(res)
}


var_between.var <- function(ss){
  #extract sample level statistics
  sum_p_i2 <- ss$sum_p_i2[1] 
  mu <- ss$mu1[1]
  A_g <- with(ss, 2*p_g*(mu_g - mu))
  B_g <- with(ss, mu_g^2 - 2*mu*mu_g)
  res <- sum(A_g^2 * ss$var_mu_g)
  res <- res + calc_p_term(p = ss$p_g, const = B_g, sum_p_i2 = sum_p_i2)
  return(res)
}


var_within.var <- function(ss){
  #extract sample level statistics
  sum_p_i2 <- ss$sum_p_i2[1]
  res <- with(ss, sum(var_sigma2_g * p_g^2))
  res <- res + calc_p_term(p = ss$p_g, const = ss$var_sigma2_g, sum_p_i2)
  return(res)
}

var_decomp <- function(y, x, wgt = rep(1, length(y))){
  S <- suf_stat(y, x, wgt)
  res <- c(between = between.var(S),
           within = within.var(S),
           between_se = sqrt(var_between.var(S)),
           within_se = sqrt(var_within.var(S)))
  return(res)
}

wtd_var <- function(y, wgt){
  return(wtd.mean(y^2, wgt) - wtd.mean(y, wgt)^2)
}