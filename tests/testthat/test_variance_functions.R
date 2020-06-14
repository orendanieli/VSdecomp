library(VSdecomp)

set.seed(123)

test_that("variance components are correct", {
  n <- 10000
  wage_men <- rnorm(n/2, 0, 1)
  wage_women <- rnorm(n/2, 1, 1)
  w_men <- rep(1, n/2)
  w_women <- rep(2, n/2)
  theo_within <- 1
  theo_between <- 2/9
  ss <- var_suf_stat(y = c(wage_women, wage_men),
                     x = c(rep("women", n/2), rep("men", n/2)),
                     wgt = c(w_women, w_men))
  expect_true(abs(theo_within - var_within(ss)) < 0.01 & 
                abs(theo_between - var_between(ss)) < 0.01)
})