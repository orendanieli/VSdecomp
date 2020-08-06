#' Linear Projection
#'
#' this function projects the standardized version of y onto all the variables in X.list 
#' (so that \eqn{Y = \beta*X + \epsilon})
#' and returns
#' 
#' @param y a character specifing the name of the outcome variable (e.g. "wage").
#' @param X.list a list containing the components 
#' @param data a data frame with all the variables specified in X.list and y.
#' @param wgt an optional vector of weights.
#' @param comp.names an optional vector specifing name for each component. should be the
#'                   same length as X.list. 

linear_projection <- function(y, X.list, data, 
                              wgt = rep(1, length(y)),
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
  n_obs <- nrow(data)
  n_comp <- length(X.list)
  res <- matrix(nrow = n_obs, ncol = n_comp + 1)
  for(i in 1:n_comp){
    #gen data with constants except for a specific component
    #specifically, take the first row as a constant
    tmp <- data[rep(1, n_obs), all_x, drop = F]
    comp <- X.list[[i]]
    tmp[,comp] <- data[,comp]
    res[,i] <- predict(lin_model, newdata = tmp)
  }
  #normalize (we do this because we want to eliminate the constants)
  res[,1:n_comp] <- apply(res[,1:n_comp, drop = F], 2, function(x){x - wtd.mean(x, wgt)})
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