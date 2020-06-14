n = 1000
df = data.frame(y = rnorm(n), x = as.factor(rgeom(n, 0.4)))

bla = split(df, df$x)

unlist(lapply(bla, function(g){mean(g$y)}))
