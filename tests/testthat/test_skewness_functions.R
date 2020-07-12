library(VSdecomp)

set.seed(123)

gen_data <- function(n = 10000){
  wage_men <- rchisq(n/2, 1)
  wage_women <- rnorm(n/2, 0, 1)
  w_men <- rep(1, n/2)
  w_women <- rep(2, n/2)
  res <- data.frame(y = c(wage_women, wage_men),
                    x = c(rep("women", n/2), rep("men", n/2)),
                    wgt = c(w_women, w_men))
  return(res)
}

test_that("skewness components are correct", {
  dat <- gen_data(10000)
  theo_within <- sqrt(8) / 3
  theo_between <- 1 / sqrt(2)
  theo_cov <- 2/9
  ss <- suf_stat(y = dat$y,
                     x = dat$x,
                     wgt = dat$wgt)
  sigma2 <- wtd_var(dat$y, dat$wgt)
  within_pack <- (sigma2^-1.5) * within.skewness(ss)
  between_pack <- (sigma2^-1.5) * between.skewness(ss)
  cov_pack <- (sigma2^-1.5) * cov.skewness(ss)
  expect_true(abs(theo_within - within_pack) < 0.01 & 
              abs(theo_between - between_pack) < 0.01 &
              abs(theo_cov - cov_pack) < 0.01)
})