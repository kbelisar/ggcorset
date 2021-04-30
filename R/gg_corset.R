#' CORSET PLOT
#'
#' This function visualizes a corset plot in wide format.
#' @name gg_corset
#' @param data The name of the data frame.
#' @param y_var1 The name of measured variable at time 1.
#' @param y_var2 The name of measured variable at time 2.
#' @param group The name of units measured at each time point such as 'ID'.
#' @param c_var The name of variable to visualize by line colour, such as percent change.
#' @param vio_fill The fill colour of the half violins. Optional (defaults to a soft black).
#' @examples
#' \dontrun{
#' wide.df <- c(id = c(1,2,3,4,5),
#'              time1 = c(3,4,7,5,6),
#'              time2  = c(5,5,7,3,0)),
#'              change = c(28.57,14.29,0,-28.57,-85.71))
#' gg_corset(data = wide.df, y_var1 = "time1",
#'           y_var2 = "time2", group = "id", c_var = "change")
#' }
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 ggplot geom_point geom_line aes position_dodge position_nudge theme_classic
#' @importFrom gghalves geom_half_violin
#' @importFrom ggstance position_dodgev
#' @importFrom stats reshape
#' @export

globalVariables(c("x_axis", "y"))

## FOR WIDE-FORM DATA
gg_corset <- function(data, y_var1, y_var2, group, c_var, vio_fill = NA) {

  data <- as.data.frame(data)
  data$y_var1 <- data[,y_var1]
  data$y_var2 <- data[,y_var2]
  data$group <- data[,group]
  data$c_var <- data[,c_var]

  data.long <- reshape(as.data.frame(data),
                       idvar = c(group),
                       timevar = "x_axis",
                       varying = c("y_var1","y_var2"),
                       sep = "_",
                       direction = "long")

  vio_fill <- ifelse(is.na(vio_fill),"#0F0F0F",vio_fill)

  ### plot
  corset_plot <- ggplot(data = data.long, aes(x = x_axis, y = y)) +

    geom_point(data = data.long, mapping = aes(x = x_axis, y = y),
               position=position_dodge(width=0.05),
               size = 0.5, alpha = 0, show.legend = F) +

    geom_line(mapping = aes(group = group, colour = c_var),
              position = ggstance::position_dodgev(height = 0.1),
              size = 0.25, alpha = 1) +

    gghalves::geom_half_violin(
      data = data.long %>% filter(x_axis == "var1"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
      position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

    gghalves::geom_half_violin(
      data = data.long %>% filter(x_axis == "var2"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
      position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

    theme_classic()


    return(corset_plot)
}
