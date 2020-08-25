#' Plot Method for 'vs_decomp' Objects 
#'
#' @param object a 'vs_decomp' object.
#' @param plot.comp a vector of up to 3 components to plot. can be either character 
#'                  with component names or numeric with component indices. other
#'                  components not specified by plot.comp are summed to one additional
#'                  component. default is to plot the first 3 components.
#' @importFrom ggplot2 ggplot aes geom_area theme scale_x_continuous
#' theme_bw ylab xlab labs geom_line geom_point
plot.vs_decomp <- function(object, 
                           plot.comp = NULL,
                           ylim = NULL){
  comp <- object$components
  type <- object$type
  moment <- object$moment
  n <- nrow(comp)
  if(n <= 1){
    stop("plot.vs_decomp isn't available for only one year")
  } 
  if(type == "linear"){
    comp <- pick_comp(comp, plot.comp)
    comp_names <- colnames(comp)
  } else {
    if(moment == "variance"){
      comp_names <- c("Between", "Within")
    } else {
      comp_names <- c("Between", "Within", "3Cov")
    }
  }
  base_comp <- comp[1, ,drop = F]
  diff <- t(apply(comp, 1, function(x){x - base_comp}))
  total <- apply(diff, 1, sum)
  colnames(diff) <- comp_names
  diff <- as.data.frame(diff)
  years_vec <- as.integer(rownames(comp))
  diff$year <- years_vec
  melted_diff <- reshape::melt(diff, id = "year")
  diff$total <- total
  diff$leg_label = ifelse(moment == "variance", "Total Variance", "Total Skewness")
  colnames(melted_diff)[2] <- "Component"
  graph <- ggplot(melted_diff, aes(x=year, y=value)) +
    geom_area(aes(fill = Component), col='black') + 
    theme(legend.position="right") +
    scale_x_continuous(breaks = years_vec) +
    theme_bw()+
    ylab(label="Change") +
    xlab(label="") +
    geom_line(data = diff, aes(year, total, linetype = leg_label), size = 1) +
    geom_point(data = diff, aes(year, total, shape = leg_label), size = 2) +
    labs(linetype="", shape="")
  graph
}


#this function picks up to 3 components and sums all other to one component called
#"all other terms"
pick_comp <- function(comp.table, comp.list = NULL){
  if(is.null(comp.list)){
    comp_ind <- 1:3
    n <- 3
  } else {
    n <- length(comp.list)
    if(n > 3){
      stop("no more than 3 components are allowed")
    }
    if(is.numeric(comp.list)){
      comp_ind <- comp.list
    } else {
      comp_ind <- which(colnames(comp.table) == comp.list)
    }
  }
  res <- comp.table[,comp_ind]
  all_other <- apply(comp.table[,-comp_ind], 1, sum)
  res <- cbind(res, all_other)
  colnames(res)[n + 1] <- "all other terms"
  return(res)
}





