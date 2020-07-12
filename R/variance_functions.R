#' Calculates sufficient statistics for the variance decomposition
#'
#'
#' @importFrom Hmisc wtd.mean 
suf_stat <- function(y, x, wgt = rep(1, length(y))){
  d <- data.frame(y, x, wgt)
  d$x <- as.factor(d$x)
  splitted_d <- split(d, d$x)
  n_groups <- length(splitted_d)
  ss <- data.frame(mu_g = rep(NA, n_groups),
                   mu2_g = rep(NA, n_groups),
                   mu3_g = rep(NA, n_groups),
                   mu4_g = rep(NA, n_groups))
  for(i in 1:4){
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
  ss$sigma2 <- wtd_var(y, wgt)
  return(ss)
}

within.var <- function(ss){
  return(wtd.mean(ss$sigma2_g, ss$p_g))
}

between.var <- function(ss){
  wtd.mean(ss$mu_g^2, ss$p_g) - wtd.mean(ss$mu_g, ss$p_g)^2
}


var_between.var <- function(ss){
  #extract sample level statistics
  sum_p_i2 <- ss$sum_p_i2[1]
  mu <- ss$mu1[1]
  A_g <- with(ss, 2*p_g*(mu_g - mu))
  B_g <- with(ss, mu_g^2 - 2*mu*mu_g)
  res <- sum(A_g^2 * ss$var_mu_g)
  B_mat <- B_g %*% t(B_g)
  p_mat <- with(ss, p_g %*% t(p_g))
  diag(p_mat) <- with(ss, p_g * (1 - p_g))
  #multiply cov by -1
  p_mat <- -1*p_mat
  diag(p_mat) <- -1*diag(p_mat)
  res <- res + sum(p_mat * B_mat) * sum_p_i2
  return(res)
}


var_within.var <- function(ss){
  #extract sample level statistics
  sum_p_i2 <- ss$sum_p_i2[1]
  res <- with(ss, sum(var_sigma2_g * p_g^2))
  B_mat <- with(ss, var_sigma2_g %*% t(var_sigma2_g))
  p_mat <- with(ss, p_g %*% t(p_g))
  diag(p_mat) <- with(ss, p_g * (1 - p_g))
  #multiply cov by -1
  p_mat <- -1*p_mat
  diag(p_mat) <- -1*diag(p_mat)
  res <- res + sum(p_mat * B_mat) * sum_p_i2
  return(res)
}

var_decomp <- function(y, x, wgt = rep(1, length(y))){
  S <- suf_stat(y, x, wgt)
  res <- c(between = between.var(S),
           within = within.var(S),
           betweed_sd = sqrt(var_between.var(S)),
           within_sd = sqrt(var_within.var(S)))
  return(res)
}

wtd_var <- function(y, wgt){
  return(wtd.mean(y^2, wgt) - wtd.mean(y, wgt)^2)
}