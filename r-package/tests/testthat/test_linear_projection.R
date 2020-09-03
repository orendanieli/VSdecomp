library(VSdecomp)

#gen data
n <- 1000
X <- matrix(rnorm(n*3, mean = 3), ncol = 3)
X <- cbind(X, rbinom(n, 30, 0.5))
colnames(X) <- c("x1", "x2", "x3", "x4")
beta <- c(1,2,3, 2)
wage <- X %*% beta + rnorm(n)
dat <- as.data.frame(cbind(wage, X))
dat$x4 <- as.factor(dat$x4)
colnames(dat)[1] <- "wage"
wgt <- rexp(n, 0.5)

test_that("linear_projection returns valid output", {
  res <- linear_projection(wage, X.list = list("x1", c("x2", "x3", "x4")),
                           data = dat, wgt= wgt)
  y <- apply(res, 1, sum)
  wage <- standardize(dat$wage , wgt)
  diff <- y - wage
  #y should be equal to wage up to a constant
  expect_true(wtd_var(diff, wgt) < 10^-10)
})