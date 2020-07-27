library(VSdecomp)

set.seed(43)
skip = T

gen_data <- function(n = 10000, for.var = F){
  if(for.var){
    wage_men <- rnorm(n/2, 0, 1)
    wage_women <- rnorm(n/2, 1, 2)
  } else {
    wage_men <- rchisq(n/2, 1)
    wage_women <- rnorm(n/2, 0, 1)
  }
  w_men <- rep(1, n/2)
  w_women <- rep(2, n/2)
  res <- data.frame(y = c(wage_women, wage_men),
                    x = c(rep("women", n/2), rep("men", n/2)),
                    wgt = c(w_women, w_men))
  return(res)
}

test_that("skewness components are correct", {
  dat <- gen_data(100000)
  theo_within <- 2^1.5 * (sqrt(8) / 3)
  theo_between <- 2 / 27
  theo_cov <- 2/9
  ss <- suf_stat(y = dat$y,
                     x = dat$x,
                     wgt = dat$wgt)
  within_pack <- within.skew(ss)
  between_pack <- between.skew(ss)
  cov_pack <- cov.skew(ss)
  expect_true(abs(theo_within - within_pack) < 0.1 & 
              abs(theo_between - between_pack) < 0.1 &
              abs(theo_cov - cov_pack) < 0.1)
})

test_that("SE of the between component is correct", {
  if(skip){
    return(TRUE)
  }
  B <- 1000
  n <- 100000
  bet_vec <- rep(NA, B)
  for(i in 1:B){
    dat <- gen_data(n, for.var = T)
    ss <- suf_stat(y = dat$y,
                   x = dat$x,
                   wgt = dat$wgt)
    sigma2 <- wtd_var(dat$y, dat$wgt)
    bet_vec[i] <- between.skew(ss) * sigma2^-1.5
  }
  emp_se <- sd(bet_vec)
  dat <- gen_data(n, for.var = T)
  ss <- suf_stat(y = dat$y,
                 x = dat$x,
                 wgt = dat$wgt)
  W <- between.skew(ss)
  app_se <- sqrt(var_between.skew(ss, W))
  expect_true(abs(app_se - emp_se) / app_se < 0.05)
})

test_that("SE of the within component is correct", {
  if(skip){
    return(TRUE)
  }
  B <- 1000
  n <- 100000
  bet_vec <- rep(NA, B)
  for(i in 1:B){
    dat <- gen_data(n, for.var = T)
    ss <- suf_stat(y = dat$y,
                   x = dat$x,
                   wgt = dat$wgt)
    sigma2 <- wtd_var(dat$y, dat$wgt)
    bet_vec[i] <- within.skew(ss) * sigma2^-1.5
  }
  emp_se <- sd(bet_vec)
  dat <- gen_data(n, for.var = T)
  ss <- suf_stat(y = dat$y,
                 x = dat$x,
                 wgt = dat$wgt)
  W <- within.skew(ss)
  app_se <- sqrt(var_within.skew(ss, W))
  expect_true(abs(app_se - emp_se) / app_se < 0.05)
})

skip = F
test_that("SE of the cov component is correct", {
  if(skip){
    return(TRUE)
  }
  B <- 1000
  n <- 100000
  bet_vec <- rep(NA, B)
  for(i in 1:B){
    dat <- gen_data(n, for.var = T)
    ss <- suf_stat(y = dat$y,
                   x = dat$x,
                   wgt = dat$wgt)
    sigma2 <- wtd_var(dat$y, dat$wgt)
    bet_vec[i] <- cov.skew(ss) * sigma2^-1.5
  }
  emp_se <- sd(bet_vec)
  dat <- gen_data(n, for.var = T)
  ss <- suf_stat(y = dat$y,
                 x = dat$x,
                 wgt = dat$wgt)
  W <- cov.skew(ss)
  app_se <- sqrt(var_cov.skew(ss, W))
  print(app_se)
  print(emp_se)
  expect_true(abs(app_se - emp_se) / app_se < 0.05)
})