n <- 5000
year = c()
y <- c()
x = c()
for(v in 1996:2002){
  wage_men <- rnorm(n, mean = 1.5 + (v - 1998) / 4, sd = 1 + (v - 1998) / 4)
  wage_women <- rnorm(n, mean = 1, sd = 1 )
  y = c(y, c(wage_men, wage_women))
  x = c(x, c(rep("women", n), rep("men", n)))
  year = c(year, rep(v, 2*n))
}

X = matrix(x, ncol = 1)

tmp <- vs_decomp(y = y,
                 X = X,
                 moment = "variance",
                 year = year)
plot(tmp)
summary(tmp)
