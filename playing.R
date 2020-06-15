n <- 10000
wage_men <- rnorm(n/2, 0, 1)
wage_women <- rnorm(n/2, 1, 1)
w_men <- rep(1, n/2)
w_women <- rep(2, n/2)
theo_within <- 1
theo_between <- 2/9
S <- suf_stat.var(y = c(wage_women, wage_men),
                   x = c(rep("women", n/2), rep("men", n/2)),
                   wgt = c(w_women, w_men))

between.var(S)
var_between.var(S)

