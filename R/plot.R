#' Calculates sufficient statistics for the variance decomposition
#'
#' @importFrom ggplot2 ggplot aes geom_area theme scale_x_continuous
#' theme_bw ylab xlab labs geom_line geom_point
plot.var_decomp <- function(object, ylim = NULL){
  comp <- object$components
  n <- nrow(comp)
  if(n <= 1){
    stop("plot.var_decomp isn't available for only one year")
  } 
  base_comp <- comp[1, ,drop = F]
  diff <- t(apply(comp, 1, function(x){x - base_comp}))
  colnames(diff) <- c("Between", "Within")
  total <- apply(diff, 1, sum)
  diff <- as.data.frame(diff)
  years_vec <- as.integer(rownames(comp))
  diff$year <- years_vec
  melted_diff <- reshape::melt(diff, id = "year")
  diff$total <- total
  diff$leg_label = "Total Variance"
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