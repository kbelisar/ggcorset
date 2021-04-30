#' CORSET PLOT
#'
#' This function visualizes a corset plot in long format.
#' @name gg_corset_elongated
#' @param data The name of the data frame.
#' @param x_var The name of the x_axis variable.
#' @param x_vals The values of the two time points.
#' @param y_var The repeated measure variable name.
#' @param group The name of units measured at each time point such as 'ID'.
#' @param c_var The name of variable to visualize by line colour, such as percent change.
#' @param vio_fill The fill colour of the half violins. Optional (defaults to a soft black).
#' @examples
#' \dontrun{
#' long.df <- c(time = c("pre","post","pre","post","pre","post",
#'              days  = c(3,5,4,0,7,7)),
#'              change = c(28.57,28.57,-57.14,-57.14,0,0),
#'              id = c(1,1,2,2,3,3))
#' gg_corset_elongated(data = long.df, x_var = "time",
#'                    x_vals = c("pre","post"), y_var = "days",
#'                    group = "id", c_var = "change")
#' }
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 ggplot geom_point geom_line aes position_dodge position_nudge theme_classic
#' @importFrom gghalves geom_half_violin
#' @importFrom ggstance position_dodgev
#' @importFrom stats reshape
#' @export


## FOR LONG-FORM DATA
gg_corset_elongated <- function(data, x_var, x_vals, y_var, group, c_var, vio_fill = NA) {

  data <- as.data.frame(data)
  data$x_var <- data[,x_var]
  data$x_var <- factor(data$x_var, levels = c(x_vals))
  data$y_var <- data[,y_var]
  data$group <- data[,group]
  data$c_var <- data[,c_var]

  vio_fill <- ifelse(is.na(vio_fill),"#0F0F0F",vio_fill)

  ### plot
  corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

    geom_point(data = data, mapping = aes(x = x_var, y = y_var),
               position=position_dodge(width=0.05),
               size = 0.5, alpha = 0, show.legend = F) +

    geom_line(mapping = aes(group = group, colour = c_var),
              position = ggstance::position_dodgev(height = 0.1),
              size = 0.25, alpha = 1) +

    gghalves::geom_half_violin(
      data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
      position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

    gghalves::geom_half_violin(
      data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
      position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

    theme_classic()


  return(corset_plot)
}
