#' THEME_GGCORSET
#'
#' This function offers a ggplot theme to make visualizations more polished.
#' @name theme_ggcorset
#' @return ggplot2 theme
#' @examples
#'
#' wide.df <- data.frame(id = c(1,2,3,4,5),
#'              time1 = c(3,4,7,5,6),
#'              time2  = c(5,5,7,3,0),
#'              change = c(28.57,14.29,0,-28.57,-85.71))
#'
#' plot1 <- gg_corset(data = wide.df, y_var1 = "time1", y_var2 = "time2",
#'           group = "id", c_var = "change")
#'
#' plot1 + theme_ggcorset()
#'
#' @importFrom ggplot2 %+replace% theme element_text element_rect element_line margin unit
#' @export

theme_ggcorset <- function() {

  theme_classic() %+replace%
    theme(plot.title = element_text(size = 20,face = "bold", hjust = 0.5, vjust = 1, margin = margin(b = 3)),
          plot.subtitle = element_text(size = 13, hjust = 0.5, vjust = 0),
          panel.border = element_rect(size = 1, fill = NA),
          legend.direction = "horizontal",
          legend.position = "top",
          legend.key.width = unit(15, "mm"),
          legend.key.height = unit(3, "mm"),
          legend.title = element_text(size = 11, face = "bold", vjust = 1),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(size = 15, face = "bold", colour = "black", hjust = 0.5, vjust = 0),
          axis.text.y = element_text(size = 15, face = "bold", colour = "black", hjust = 0, margin = margin(r = 2.5)),
          axis.title.x = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 0, margin = margin(t = 3)),
          axis.title.y = element_text(size = 18, face = "bold", hjust = 0.5, angle = 90, margin = margin(r = 10)),
          axis.ticks.length = unit(2, "mm"),
          axis.ticks = element_line(size = 0.75),
          strip.text = element_text(size = 13, face = "bold", vjust = 0.5, margin = margin(t = 5, b = 5)))
}
