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
#' @param year an optional vector of years. if provided, the projection is done for each year separately.
#' @param comp.names an optional vector specifying name for each component.
#' @return a matrix with the (centered) components specified by X.list + residuals. Note that each row is summed (up to 
#'         a constant) to the standardized version of y, and each column to 0 (both by year).
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
#' @importFrom stats as.formula lm predict
#' @export

linear_projection <- function(y, X.list, data, 
                              wgt = rep(1, nrow(data)),
                              year = rep(1, nrow(data)),
                              comp.names = NULL){ 
  dep_var <- ifelse(is.character(y), y, as.character(deparse(substitute(y))))
  year_val <- unique(year)
  res <- matrix(nrow = nrow(data), ncol = length(X.list) + 1)
  for(v in year_val){
    ind <- year == v
    res[ind,] <- get_comp(dep_var, X.list, data[ind,], wgt[ind])
  }
  #add names
  if(!is.null(comp.names) & length(comp.names) == length(X.list)){
    colnames(res) <- c(comp.names, "epsilon")
  } else {
    colnames(res) <- c(create_names(X.list), "epsilon")
  }
  return(res)
}

#this is the basic function that calculates the components. 
get_comp <- function(dep_var, X.list, data, wgt){
  #standradize y 
  data[,dep_var] <- standardize(data[,dep_var], wgt)
  all_x <- unlist(X.list)
  are_factors <- unlist(lapply(data[,all_x], is.factor)) #note that this cannot work with interactions (:)
  if(any(are_factors)){
    terms_obj <- get_fe_terms(dep_var, all_x, are_factors, data, wgt)
    epsilon <- terms_obj$epsilon
    fe_comp <- terms_obj$fe_terms
    if(!all(are_factors)){
      #subtract fixed effects and project on other variables
      data[,dep_var] <- data[,dep_var] - terms_obj$sumFE
      all_comp <- get_terms(dep_var, all_x[!are_factors], data, wgt)$terms
      all_comp <- cbind(all_comp, fe_comp)
    } else {
      all_comp <- fe_comp
    }
  } else {
    terms_obj <- get_terms(dep_var, all_x, data, wgt)
    all_comp <- terms_obj$terms
    epsilon <- terms_obj$epsilon
  }
  #aggregate components (according to X.list)
  n_comp <- length(X.list)
  res <- matrix(nrow = nrow(data), ncol = n_comp + 1)
  for(i in 1:n_comp){
    comp <- X.list[[i]]
    res[,i] <- apply(all_comp[,comp, drop = FALSE], 1, sum)
  }
  #add epsilon (=residual)
  res[,n_comp + 1] <- epsilon
  return(res)
}

#estimate the model y ~ all_x and return model terms + model residuals
get_terms <- function(dep_var, all_x, data, wgt = rep(1, nrow(data))){
  #create formula
  indep_vars <- paste(all_x, collapse='+')
  form <- as.formula(paste(dep_var, '~', indep_vars))
  lin_model <- lm(formula = form, data = data, weights = wgt)
  #here we normalize the terms 
  #(this is important cause later we calculate cov and its convenient that all variables are zero mean)
  coeffs <- lin_model$coefficients[-1] #remove intercept
  X <- data[,all_x, drop = FALSE]
  terms <- t(coeffs * t(X))
  terms <- apply(terms, 2, function(x){x - wtd.mean(x, wgt)})
  epsilon <- lin_model$residuals
  return(list(terms = terms, epsilon = epsilon))
}


#same as get_term but return only FE terms
get_fe_terms <- function(dep_var, all_x, are_factors, data, wgt){
  #prepare formula
  num_var <- ifelse(all(are_factors), "0",
                    paste(all_x[!are_factors], collapse = "+"))
  cat_var <- paste(all_x[are_factors], collapse = "+")
  indep_vars <- paste(num_var, cat_var, sep = " | ")
  form <- as.formula(paste(dep_var, '~', indep_vars))
  fe_model <- lfe::felm(form, data = data, weights = wgt)
  #get FE terms
  fe_table <- lfe::getfe(fe_model)
  num_fe <- sum(are_factors)
  fe_terms <- matrix(ncol = num_fe, nrow = nrow(data)) 
  for(i in 1:num_fe){
    x <- all_x[are_factors][i]
    left <- data[,x, drop = FALSE]
    right <- fe_table[fe_table[,"fe"] == x, c("effect", "idx")]
    tmp <- keeping_order(left, merge, y = right, by.x = x, by.y = "idx")
    fe_terms[,i] <- tmp$effect
    fe_terms[,i] <- fe_terms[,i] - wtd.mean(fe_terms[,i], wgt)
  }
  colnames(fe_terms) <- all_x[are_factors]
  return(list(fe_terms = fe_terms, epsilon = fe_model$residuals, sumFE = fe_model$r.residuals))
}


#keep data order after fn manipulation
keeping_order <- function(data, fn, ...) { 
  col <- ".sortColumn"
  data[,col] <- 1:nrow(data) 
  out <- fn(data, ...) 
  if (!col %in% colnames(out)) stop("Ordering column not preserved by function") 
  out <- out[order(out[,col]),] 
  out[,col] <- NULL 
  out 
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