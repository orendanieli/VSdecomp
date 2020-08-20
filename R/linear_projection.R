
#' Linear Projection
#'
#' estimates the linear model \eqn{y = \beta*X + \epsilon} and returns its linear
#' components, grouped according to X.list.
#' 
#' @param y a character specifying the name of the outcome variable (e.g. "wage"). 
#'           Note that this variable is standardized before it's projected onto X.
#' @param X.list a list containing the names of all the variables needed for the linear projection, grouped
#'               according to the components will later be used in the skewness decomposition. For example:
#'               for X.list = list("x1", c("x2", "x3")) the following components are returned: \eqn{\beta1X1},
#'               (\eqn{\beta2X2+ \beta3X3}), \eqn{\epsilon}. Interactions are defined as usual, e.g.:
#'               "x1:x2" (cuurently only second-order interactions are allowed).
#' @param data a data frame with all the variables specified in X.list and y.
#' @param wgt an optional vector of weights.
#' @param comp.names an optional vector specifying name for each component. should be the
#'                   same length as X.list. 
#' @return a matrix with the (centered) components specified by X.list + residuals. Note that each row is summed (up to 
#'         a constant) to the standardized version of y, and each column to 0.
#' @examples
#' #gen data
#' n <- 1000
#' X <- matrix(rnorm(n*3), ncol = 3)
#' colnames(X) <- c("x1", "x2", "x3")
#' beta <- c(1,2,3)
#' wage <- X %*% beta + rnorm(n)
#' dat <- as.data.frame(cbind(wage, X))
#' colnames(dat)[1] <- "wage"
#' res <- linear_projection("wage", X.list = list("x1", c("x2", "x3"), "x2:x3"), data = dat)
#' #each row is summed (up to a constant) to the standardized wage:
#' stand_wage <- (wage - mean(wage)) / sd(wage)
#' diff <- apply(res, 1, sum) - stand_wage
#' summary(diff)

#' @export

linear_projection <- function(y, X.list, data, 
                              wgt = rep(1, nrow(data)),
                              comp.names = NULL){
  dep_var <- ifelse(is.character(y), y, as.character(deparse(substitute(y))))
  #standradize y 
  data[,dep_var] <- standardize(data[,dep_var], wgt)
  all_x = unlist(X.list)
  #create formula
  indep_vars <- paste(all_x, collapse='+')
  form <- as.formula(paste(dep_var, '~', indep_vars))
  lin_model <- lm(formula = form, data = data, weights = wgt)
  #aggregate components (according to X.list)
  #note that here we already get centerd terms, so no normalization is needed. 
  #(this is important cause later we calculate cov and its convenient that all variables are zero mean)
  all_comp <- predict(lin_model, type = "term")
  n_obs <- nrow(data)
  n_comp <- length(X.list)
  res <- matrix(nrow = n_obs, ncol = n_comp + 1)
  for(i in 1:n_comp){
    comp <- X.list[[i]]
    res[,i] <- apply(all_comp[,comp, drop = F], 1, sum)
  }
  #add epsilon (=residual)
  res[,n_comp + 1] <- lin_model$residuals
  #add names
  if(!is.null(comp.names) & length(comp.names) == n_comp){
    colnames(res) <- c(comp.names, "residual")
  } else {
    colnames(res) <- c(create_names(X.list), "residual")
  }
  return(res)
}

#create names from X.list object
create_names <- function(X.list){
  nms <- rep(NA, length(X.list))
  i <- 1
  for(x in X.list){
    if(length(x) == 1){
      nms[i] <- x
    } else {
      nms[i] <- paste(x, collapse = "_")
    }
    i <- i + 1
  }
  return(nms)
}

standardize <- function(y, wgt){
   (y - wtd.mean(y, wgt)) / sqrt(wtd.mean(y^2, wgt) - wtd.mean(y, wgt)^2)
}

