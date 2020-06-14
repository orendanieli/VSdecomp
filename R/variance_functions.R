#calculates sufficient statistics for the variance decomposition
var_suf_stat <- function(y, x, wgt = rep(1, length(y))){
  d <- data.frame(y, x, wgt)
  d$x <- as.factor(d$x)
  splitted_d <- split(d, d$x)
  sigma_g <- lapply(splitted_d, function(g){wtd.var(g$y, g$wgt)})
  mu_g <- lapply(splitted_d, function(g){wtd.mean(g$y, g$wgt)})
  sum_w <- sum(wgt)
  p_g <- lapply(splitted_d, function(g){sum(g$wgt) / sum_w})
  ss <- data.frame(mu_g = unlist(mu_g),
                           sigma_g = unlist(sigma_g),
                           p_g = unlist(p_g))
  return(ss)
}

var_within <- function(ss){
  return(wtd.mean(ss$sigma_g, ss$p_g))
}

var_between <- function(ss){
  wtd.mean(ss$mu_g^2, ss$p_g) - wtd.mean(ss$mu_g, ss$p_g)^2
}
