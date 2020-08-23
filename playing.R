library(MASS)
#playing for p == 1
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
                 moment = "var",
                 year = year)
plot(tmp)
summary(tmp)

#playing for p > 1
n = 10000
X = mvrnorm(n, mu = c(2, 1), Sigma = matrix(c(1, 0.1, 0.1, 1), ncol = 2))
colnames(X) = c("ind", "occ")
wage = X %*% c(3,1) + rnorm(n)
dat = cbind.data.frame(wage, X)
X = linear_projection(y = "wage", X.list = list("ind", "occ"), data = dat)
year = sample(c(1996, 1997), size = n, replace = T)
bla = vs_decomp(X = X, year = year)


bla2 = linear_skew_decomp(X = X)




