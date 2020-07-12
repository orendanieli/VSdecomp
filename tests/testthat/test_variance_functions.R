library(VSdecomp)

set.seed(123)

gen_data <- function(n = 10000){
  wage_men <- rnorm(n/2, 0, 1)
  wage_women <- rnorm(n/2, 1, 1)
  w_men <- rep(1, n/2)
  w_women <- rep(2, n/2)
  res <- data.frame(y = c(wage_women, wage_men),
                    x = c(rep("women", n/2), rep("men", n/2)),
                    wgt = c(w_women, w_men))
  return(res)
}

test_that("variance components are correct", {
  dat <- gen_data(10000)
  theo_within <- 1
  theo_between <- 2/9
  ss <- suf_stat(y = dat$y,
                     x = dat$x,
                     wgt = dat$wgt)
  expect_true(abs(theo_within - within.var(ss)) < 0.01 & 
                abs(theo_between - between.var(ss)) < 0.01)
})

test_that("SE of the between component is correct", {
  B <- 1000
  n <- 10000
  bet_vec <- rep(NA, B)
  for(i in 1:B){
    dat <- gen_data(n)
    ss <- suf_stat(y = dat$y,
                       x = dat$x,
                       wgt = dat$wgt)
    bet_vec[i] <- between.var(ss)
  }
  emp_se <- sd(bet_vec)
  dat <- gen_data(n)
  ss <- suf_stat(y = dat$y,
                     x = dat$x,
                     wgt = dat$wgt)
  app_se <- sqrt(var_between.var(ss))
  expect_true(abs(app_se - emp_se) / app_se < 0.05)
})

test_that("SE of the within component is correct", {
  B <- 1000
  n <- 10000
  with_vec <- rep(NA, B)
  for(i in 1:B){
    dat <- gen_data(n)
    ss <- suf_stat(y = dat$y,
                       x = dat$x,
                       wgt = dat$wgt)
    with_vec[i] <- within.var(ss)
  }
  emp_se <- sd(with_vec)
  dat <- gen_data(n)
  ss <- suf_stat(y = dat$y,
                     x = dat$x,
                     wgt = dat$wgt)
  app_se <- sqrt(var_within.var(ss))
  expect_true(abs(app_se - emp_se) / app_se < 0.05)
})