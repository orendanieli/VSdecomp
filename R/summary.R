summary.vs_decomp <- function(object, r = 3){
  comp <- object$components
  cat("Moment:", object$moment)
  num_year <- nrow(comp)
  cat("\n", "\n","Number of Observations:", "\n")
  print(object$N)
  comp_var <- object$components_se^2
  cat("\n","Baseline:", "\n")
  base_comp <- comp[1, ,drop = F]
  print(round(base_comp, r))
  if(num_year == 1){
    return(cat("\n", "Note: only one year was provided"))
  }
  #set parameters by moment
  if(object$moment == "variance"){
    col_num <- 2
    col_names <- c("between", "within")
  } else {
    col_num <- 3
    col_names <- c("between", "within", "3cov")
  }
  cat("\n", 
      "Changes since base year: difference, (difference SE) and [share of total change]",
     "\n", "\n")
  diff <- t(apply(comp, 1, function(x){x - base_comp}))
  diff <- round(diff, r)
  base_var <- comp_var[1, ,drop = F]
  diff_sd <- t(apply(comp_var, 1, function(x){sqrt(x + base_var)}))
  diff_sd <- round(diff_sd, r)
  #add ()
  diff_sd <- apply(diff_sd, c(1, 2), function(x){paste("(",x,")", sep = "")})
  total_change <- apply(diff, 1, sum)
  comp_share <- round(diff / total_change, r)
  #add []
  comp_share <- apply(comp_share, c(1, 2), function(x){paste("[",x,"]", sep = "")})
  #this is a nice trick to combine the matrices
  output <- matrix(t(cbind(diff, diff_sd, comp_share)), ncol = col_num, byrow = T) 
  colnames(output) <- col_names
  #output <- as.data.frame(output)
  #create rownames by "pushing" c("", "") after each year
  rnames <- rownames(comp)
  rnames <- rbind(rnames, rep("", num_year), rep("", num_year))
  rownames(output) <- as.vector(rnames)
  noquote(output[-(1:3),])
}