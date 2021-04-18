#' Plot Method for 'vs_decomp' Objects 
#'
#' @param x a 'vs_decomp' object.
#' @param plot.comp a vector of up to 3 components to plot. can be either character 
#'                  with component names or numeric with component indices. other
#'                  components not specified by plot.comp are summed to one additional
#'                  component. default is to plot the first 3 components. another option
#'                  is to additionally specify "all other terms" in order to control for the position 
#'                  of each component. (relevant only for the linear decomposition).
#' @param comp.labels a vector of the same length as `plot.comp`, 
#'                   which provides the labels to be shown in the graph. default is
#'                   to use the original component names. (relevant only for
#'                  the linear decomposition).
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
                           comp.labels = NULL,
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
  if((length(comp.labels) != length(plot.comp)) & (!is.null(comp.labels))){
    stop("comp.labels and plot.comp should be the same length")
  }
  if(type == "linear"){
    tmp <- pick_comp(comp, plot.comp, comp.labels)
    comp <- tmp$comp
    comp_names <- tmp$names
  } else {
    if(moment == "variance"){
      comp_names <- c("Between", "Within")
    } else {
      comp_names <- c("Between", "Within", "3COV")
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
  if(type != "linear"){
    #reorder 
    melted_diff$Component <- factor(melted_diff$Component, levels = c("Within", "Between", "3COV"))
  }
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
  print(graph)
  graph
}


#this function picks up to 3 components and sums all other to one component called
#"all other terms"
pick_comp <- function(comp.table, comp.list = NULL, comp.labels = NULL){
  if(is.null(comp.list)){
    comp_ind <- 1:3
    n <- 3
  } else {
    #in case the user provided "all other terms", we want to add it at the
    #same place as specified by the user. 
    n <- length(comp.list)
    all_other_flag <- "all other terms" %in% comp.list
    if(any(all_other_flag)){
      all_other_pos <- which(all_other_flag)
      comp.list <- comp.list[-all_other_pos]
      n <- n - 1
    } else {
      all_other_pos <- n + 1
      if(!is.null(comp.labels)){
        comp.labels <- c(comp.labels, "all other terms")
      }
    }
    if(n > 3){
      stop("no more than 3 components are allowed")
    }
    if(is.numeric(comp.list)){
      comp_ind <- comp.list
    } else {
      comp_ind <- c(NULL)
      #this is done this way in order to keep comp.list order
      for(i in 1:length(comp.list)){
        comp_ind = c(comp_ind, which(colnames(comp.table) == comp.list[i]))
      }
    }
  }
  res <- comp.table[,comp_ind]
  all_other <- apply(comp.table[,-comp_ind], 1, sum)
  #keep user order (or put all other term at the end)
  if(all_other_pos == 1){
    res <- cbind(all_other, res)
  } else if(all_other_pos > n){
    res <- cbind(res, all_other)
  } else {
    res <- cbind(res[,1:(all_other_pos-1)], all_other, res[,all_other_pos:n])
  }
  colnames(res)[all_other_pos] <- "all other terms"
  output <- list(comp = res, 
                 names = if(is.null(comp.labels)){colnames(res)} else {comp.labels})
  return(output)
}





