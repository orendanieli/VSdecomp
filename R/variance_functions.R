#' Calculates sufficient statistics for the variance decomposition
#'
#'
#' @importFrom Hmisc wtd.mean wtd.var
suf_stat.var <- function(y, x, wgt = rep(1, length(y))){
  sum_w <- sum(wgt)
  d <- data.frame(y, x, wgt, p_i = wgt / sum_w)
  d$x <- as.factor(d$x)
  splitted_d <- split(d, d$x)
  sigma_g <- lapply(splitted_d, function(g){wtd.var(g$y, g$wgt)})
  mu_g <- lapply(splitted_d, function(g){wtd.mean(g$y, g$wgt)})
  p_g <- lapply(splitted_d, function(g){sum(g$wgt) / sum_w})
  sum_p_i2_g <- lapply(splitted_d, function(g){sum(g$p_i^2)})
  ss <- data.frame(mu_g = unlist(mu_g),
                           sigma_g = unlist(sigma_g),
                           p_g = unlist(p_g),
                           sum_p_i2_g = unlist(sum_p_i2_g))
  ss$var_mu_g <- ss$sigma_g * ss$sum_p_i2_g
  return(ss)
}

within.var <- function(ss){
  return(wtd.mean(ss$sigma_g, ss$p_g))
}

between.var <- function(ss){
  wtd.mean(ss$mu_g^2, ss$p_g) - wtd.mean(ss$mu_g, ss$p_g)^2
}

#compare this to the theoretical example to check that there are no typos
var_between.var <- function(ss){
  mu <- with(ss, sum(mu_g * p_g))
  sum_p_i2 <- sum(ss$sum_p_i2_g)
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
