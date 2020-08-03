#check that this function works, and continue to vs_decomp with 
#the output of this function

linear_projection <- function(y, X.list, data, 
                              wgt = rep(1, length(y)),
                              comp.names = NULL){
  all_x = unlist(X.list)
  #create formula
  indep_vars <- paste(all_x, collapse='+')
  dep_var <- deparse(substitute(y))
  form <- as.formula(paste(dep_var, '~', indep_vars))
  lin_model <- lm(formula = form, data = data, weights = wgt)
  #aggregate components (according to X.list)
  n_obs <- nrow(data)
  n_comp <- length(X.list)
  res <- matrix(nrow = n_obs, ncol = n_comp)
  #gen data with constants except for a specific component
  #specifically, take the first row as a constant
  tmp <- data[rep(1, n_obs), all_x]
  for(i in 1:n_comp){
    comp <- X.list[i]
    tmp[,comp] <- data[,comp]
    res[,i] <- predict(lin_model, newdata = tmp)
  }
  #normalize
  res <- apply(res, 2, function(x){x - wtd.mean(x, wgt)})
  #add epsilon (=residual)
  res[,n_comp + 1] <- lin_model$residuals
  #add names
  if(!is.null(comp.names)){
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
  }
  return(nms)
}
