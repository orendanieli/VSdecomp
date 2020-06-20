n <- 30000
wage_men <- rnorm(n/3, 0, 1)
wage_women <- rnorm(2*n/3, 1, 1)
w_men <- rep(1, n/2)
w_women <- rep(1, n/2)
theo_within <- 1
theo_between <- 2/9
S <- suf_stat.var(y = c(wage_women, wage_men),
                   x = c(rep("women", 2*n/3), rep("men", n/3)),
                   wgt = c(w_women, w_men))

between.var(S)
var_between.var(S)

S = data.frame()