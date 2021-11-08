
VSdecomp: Variance and Skewness Decomposition
=============================================

[![CRAN status](https://www.r-pkg.org/badges/version/VSdecomp)](https://cran.r-project.org/package=VSdecomp)

An R package that provides decomposition methods for the skewness or the variance of a variable (e.g., wage). By breaking distribution moments into independent components, users can analyze changes in distributions across time or between groups.

Installation
------------

You can install the stable version from CRAN:
```{r, message=FALSE, eval=FALSE}
install.packages("VSdecomp")
```

Or the development version from GitHub:

``` r
#install.packages("devtools")
devtools::install_github("orendanieli/VSdecomp/r-package")
```

Usage Example
-------------

``` r
library(VSdecomp)
#generate data
n <- 1000
men <- rbinom(n, 1, 0.5)
black <- rbinom(n, 1, 0.5)
year <- c(rep(2019, n/2), rep(2020, n/2))
rwage <- function(x){
  m <- x[1]; b <- x[2]; y <- x[3];
  exp(rnorm(1, mean = 1*m + 1*b, sd = 1 + 2020 - y))
}
dat <- data.frame(men, black, year)
dat$wage <- apply(dat, 1, rwage)
#skewness decomposition by one variable:
variable <- as.matrix(dat$men)
decomp_by_gender <- vs_decomp(y = dat$wage, X = variable,
                              moment = "skew", year = dat$year)
summary(decomp_by_gender)
plot(decomp_by_gender)

#skewness decomposition by more than one variable (=linear skewness decomposition):
#first we need to decompose yearly wages into their linear components
#(check ?linear_projection for more details):
wage_linear_comp <- linear_projection(y = "wage", 
                                      X.list = list("men", "black"),
                                      data = dat, year = dat$year)
#using the linear components, we can calculate skewness decomposition:
decomp_by_mult_var <- vs_decomp(X = wage_linear_comp, 
                                year = dat$year)
#up to 3 components can be summarized or plotted:
colnames(decomp_by_mult_var$components)
#let's take "3cov(epsilon^2,men)" and "3cov(epsilon^2,black)"
summary(decomp_by_mult_var, sum.comp = c(7, 8))
plot(decomp_by_mult_var, plot.comp = c(7, 8))
```

References
----------
