#' Plot Method for 'vs_decomp' Objects 
#'
#' @param x a 'vs_decomp' object.
#' @param plot.comp a vector of up to 3 components to plot. can be either character 
#'                  with component names or numeric with component indices. other
#'                  components not specified by plot.comp are summed to one additional
#'                  component. default is to plot the first 3 components.
#' @param fill.colors colors to fill the areas. see \code{\link[ggplot2]{scale_fill_manual}}
#'                    for more details.
#' @param trunc.negative whether to truncate negative values with 0. default is TRUE.
#'                       this flag is necessary because \code{\link[ggplot2]{geom_area}},
#'                       on which the function is based, does not work well with a combination of positive
#'                       and negative values. 
#' @param ... further arguments passed to or from other methods.
#' @importFrom ggplot2 ggplot aes geom_area theme scale_x_continuous
#' theme_bw ylab xlab labs geom_line geom_point scale_fill_manual
#' @importFrom rlang .data
#' @return A ggplot object containing all the relevant information for the plot.
#' @export
plot.vs_decomp <- function(x, 
                           plot.comp = NULL,
                           fill.colors = NULL,
                           trunc.negative = TRUE, ...){
  object <- x
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
  base_comp <- comp[1, ,drop = FALSE]
  diff <- t(apply(comp, 1, function(x){x - base_comp}))
  total <- apply(diff, 1, sum)
  if(trunc.negative){
    #replace negative values with 0:
    diff[diff < 0] = 0
  }
  colnames(diff) <- comp_names
  diff <- as.data.frame(diff)
  years_vec <- as.integer(rownames(comp))
  diff$year <- years_vec
  melted_diff <- reshape::melt(diff, id = "year")
  diff$total <- total
  diff$leg_label <- ifelse(moment == "variance", "Total Variance", "Total Skewness")
  colnames(melted_diff)[2] <- "Component"
  #melted_diff$Component <- factor(melted_diff$Component, c("3Cov", "Between", "Within"))
  graph <- ggplot(melted_diff, aes(x=.data$year, y=.data$value)) +
    geom_area(aes(fill = .data$Component), col='black') + 
    theme(legend.position="right") +
    scale_x_continuous(breaks = years_vec) +
    theme_bw()+
    ylab(label="Change") +
    xlab(label="") +
    geom_line(data = diff, aes(.data$year, .data$total, linetype = .data$leg_label), size = 1) +
    geom_point(data = diff, aes(.data$year, .data$total, shape = .data$leg_label), size = 2) +
    labs(linetype="", shape="")
  if(!is.null(fill.colors)){
    graph <- graph + scale_fill_manual(values = fill.colors)
  }
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
      comp_ind <- which(colnames(comp.table) %in% comp.list)
    }
  }
  res <- comp.table[,comp_ind]
  all_other <- apply(comp.table[,-comp_ind], 1, sum)
  res <- cbind(res, all_other)
  colnames(res)[n + 1] <- "all other terms"
  return(res)
}





