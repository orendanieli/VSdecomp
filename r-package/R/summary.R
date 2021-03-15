#' Summary Method for 'vs_decomp' Objects 
#' 
#' @param object a 'vs_decomp' object.
#' @param r a number of decimal places to use.
#' @param sum.comp a vector of up to 3 components to summarize. can be either character 
#'                  with component names or numeric with component indices. other
#'                  components not specified by sum.comp are summed to one additional
#'                  component. default is to summarize the first 3 components.
#' @param ... further arguments passed to or from other methods.
#' @return No return value.
#' @export

summary.vs_decomp <- function(object, r = 3, sum.comp = NULL, ...){
  comp <- object$components
  type <- object$type
  moment <- object$moment
  if(type == "linear"){
    comp <- pick_comp(comp, sum.comp)
    col_num <- ncol(comp)
    col_names <- colnames(comp)
  }
  cat("Moment:", moment)
  num_year <- nrow(comp)
  cat("\n", "\n","Number of Observations:", "\n")
  print(object$N)
  cat("\n","Baseline:", "\n")
  base_comp <- comp[1, ,drop = F]
  print(round(base_comp, r))
  if(num_year == 1){
    return(cat("\n", "Note: only one year was provided"))
  }
  #summary for more than one year
  diff <- t(apply(comp, 1, function(x){x - base_comp}))
  diff <- round(diff, r)
  total_change <- apply(diff, 1, sum)
  comp_share <- abs(round(diff / total_change, r))
  #add []
  comp_share <- apply(comp_share, c(1, 2), function(x){paste("[",x,"]", sep = "")})
  if(type == "one variable"){
    #set parameters by moment
    if(moment == "variance"){
      col_num <- 2
      col_names <- c("between", "within")
    } else {
      col_num <- 3
      col_names <- c("between", "within", "3COV")
    }
    cat("\n", 
        "Changes since base year: difference, (difference SE) and [share of total change]",
        "\n", "\n")
    comp_var <- object$components_se^2
    base_var <- comp_var[1, ,drop = FALSE]
    diff_sd <- t(apply(comp_var, 1, function(x){sqrt(x + base_var)}))
    diff_sd <- round(diff_sd, r)
    #add ()
    diff_sd <- apply(diff_sd, c(1, 2), function(x){paste("(",x,")", sep = "")})
    #this is a nice trick to combine the matrices
    output <- matrix(t(cbind(diff, diff_sd, comp_share)), ncol = col_num, byrow = TRUE) 
    colnames(output) <- col_names
    #create rownames by "pushing" c("", "") after each year
    rnames <- rownames(comp)
    rnames <- rbind(rnames, rep("", num_year), rep("", num_year))
    rownames(output) <- as.vector(rnames)
    noquote(output[-(1:3),])
  } else {
    cat("\n", 
        "Changes since base year: difference and [share of total change]",
        "\n", "\n")
    #this is a nice trick to combine the matrices
    output <- matrix(t(cbind(diff, comp_share)), ncol = col_num, byrow = TRUE) 
    colnames(output) <- col_names
    #create rownames by "pushing" c("", "") after each year
    rnames <- rownames(comp)
    rnames <- rbind(rnames, rep("", num_year))
    rownames(output) <- as.vector(rnames)
    noquote(output[-(1:2),])
  }
}