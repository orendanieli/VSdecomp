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
#'               (\eqn{\beta2X2+ \beta3X3}), \eqn{\epsilon}. Currently interactions aren't supported,
#'               so the user should insert them manually.
#' @param data a data frame with all the variables specified in X.list and y.
#' @param wgt an optional vector of weights.
#' @param comp.names an optional vector specifying name for each component.
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
#' res <- linear_projection("wage", X.list = list("x1", c("x2", "x3")), data = dat)
#' #each row is summed (up to a constant) to the standardized wage:
#' stand_wage <- (wage - mean(wage)) / sd(wage)
#' diff <- apply(res, 1, sum) - stand_wage
#' summary(diff)

#' @export

linear_projection <- function(y, X.list, data, 
                              wgt = rep(1, nrow(data)),
                              comp.names = NULL){
  n_obs <- nrow(data)
  dep_var <- ifelse(is.character(y), y, as.character(deparse(substitute(y))))
  #standradize y 
  data[,dep_var] <- standardize(data[,dep_var], wgt)
  all_x = unlist(X.list)
  are_factor <- unlist(lapply(data[,all_x], is.factor)) #note that this cannot work with interactions (:)
  if(any(are_factor)){
    #prepare formula
    num_var <- paste(all_x[!are_factor], collapse = "+")
    cat_var <- paste(all_x[are_factor], collapse = "+")
    indep_vars <- paste(num_var, cat_var, sep = " | ")
    form <- as.formula(paste(dep_var, '~', indep_vars))
    fe_model <- lfe::felm(form, data = data, weights = wgt)
    epsilon <- fe_model$residuals
    fe_table <- lfe::getfe(fe_model)
    num_fe <- sum(are_factor)
    fe_comp <- matrix(ncol = num_fe, nrow = n_obs) 
    for(i in 1:num_fe){
      x <- all_x[are_factor][i]
      fe_comp[,i] <- fe_table[paste0(x, ".", data[, x]), "effect"]
      fe_comp[,i] <- fe_comp[,i] - wtd.mean(fe_comp[,i], wgt)
    }
    colnames(fe_comp) <- all_x[are_factor]
    #subtract fixed effects and project on other variables
    data[,dep_var] <- data[,dep_var] - fe_model$r.residuals
    all_comp <- get_terms(dep_var, all_x[!are_factor], data, wgt)$terms
    all_comp <- cbind(all_comp, fe_comp)
  } else {
    terms_obj <- get_terms(dep_var, all_x, data, wgt)
    all_comp <- terms_obj$terms
    epsilon <- terms_obj$epsilon
  }
  #aggregate components (according to X.list)
  n_comp <- length(X.list)
  res <- matrix(nrow = n_obs, ncol = n_comp + 1)
  for(i in 1:n_comp){
    comp <- X.list[[i]]
    res[,i] <- apply(all_comp[,comp, drop = F], 1, sum)
  }
  #add epsilon (=residual)
  res[,n_comp + 1] <- epsilon
  #add names
  if(!is.null(comp.names) & length(comp.names) == n_comp){
    colnames(res) <- c(comp.names, "epsilon")
  } else {
    colnames(res) <- c(create_names(X.list), "epsilon")
  }
  return(res)
}

#estimates the model y ~ all_x and returns model terms + model residuals
get_terms <- function(dep_var, all_x, data, wgt = rep(1, nrow(data))){
  #create formula
  indep_vars <- paste(all_x, collapse='+')
  form <- as.formula(paste(dep_var, '~', indep_vars))
  lin_model <- lm(formula = form, data = data, weights = wgt)
  #note that here we already get centerd terms, so no normalization is needed. 
  #(this is important cause later we calculate cov and its convenient that all variables are zero mean)
  terms <- predict(lin_model, type = "term")
  epsilon <- lin_model$residuals
  return(list(terms = terms, epsilon = epsilon))
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

#this function calculates the components of the linear skewness decomposition
#(typically this is done on the output of linear_projection())
#note: all X columns should be zero mean
linear_skew_decomp <- function(X, wgt = rep(1, nrow(X))){
  X_names <- colnames(X)
  p <- ncol(X)
  mu3 <- X^3
  mu3_names <- paste0("mu3(", X_names, ")")
  X_squared <- X^2
  #cov of second with first moment
  pair_comb <- cbind(combn(1:p, 2), combn(p:1, 2))
  cov21 <- apply(pair_comb, 2, 
                 function(pair){3 * X_squared[,pair[1]] * X[,pair[2]]})
  cov21 <- as.data.frame(cov21)
  #set names
  cov21_names <- apply(pair_comb, 2,
                       function(pair){paste0("3cov(", X_names[pair[1]], 
                                             "^2,", X_names[pair[2]], ")")})
  #3 first moments multiplication
  if (p > 2){
    triple_comb <- combn(1:p, 3)
    triple <- apply(triple_comb, 2,
                    function(tr){6 * apply(X[,tr], 1, prod)})
    #set names
    triple_names <- apply(triple_comb, 2,
                          function(tr){paste0(X_names[tr], collapse = ",")})
    triple_names <- paste0("E(", triple_names, ")")
    res <- cbind(mu3, cov21, triple)
    res_names <- c(mu3_names, cov21_names, triple_names)
  } else {
    res <-  cbind(mu3, cov21)
    res_names <- c(mu3_names, cov21_names)
  }
  res <- apply(res, 2, wtd.mean, weights = wgt)
  names(res) <- res_names
  return(res)
}