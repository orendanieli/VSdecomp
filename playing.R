n <- 100
year = c()
y <- c()
x = c()
for(v in 1996:2000){
  wage_men <- rnorm(n, 0, 1)
  wage_women <- rnorm(n, 1, 1)
  y = c(y, c(wage_men, wage_women))
  x = c(x, c(rep("women", n), rep("men", n)))
  year = c(year, rep(v, 2*n))
}

X = matrix(x, ncol = 1)

bla <- vs_decomp(y = y,
                 X = X,
                 moment = "variance",
                 year = year)
bla
