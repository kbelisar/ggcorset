#' CORSET PLOT ELONGATED
#'
#' This function visualizes a corset plot in long format.
#' @name gg_corset_elongated
#' @param data The name of the data frame.
#' @param x_var The name of the x_axis variable.
#' @param x_vals The values of the two time points.
#' @param y_var The repeated measure variable name.
#' @param group The name of units measured at each time point such as 'ID'.
#' @param c_var The name of variable to visualize by line colour, such as percent change.
#' @param eyelets Optional (default is FALSE). If set to TRUE, this will visualize standard error means (SEM) by c_var.
#' @param faceted Optional (default is FALSE). If set to true, the c_var will be faceted, with all lines visible in soft grey as a background in each facet.
#' @param vio_fill Optional (defaults to a soft black). Use to change the fill colour of the half violins.
#' @param line_size Optional. Use to change the size (thickness) of the lines which visualize the c_var. Default is 0.25.
#' @return ggplot2 graphical object
#' @examples
#'
#' long.df <- data.frame(id = c(1,1,2,2,3,3),
#'              time = c("pre","post","pre","post","pre","post"),
#'              days  = c(3,5,4,0,7,7),
#'              change = c(28.57,28.57,-57.14,-57.14,0,0),
#'              direction = c("increase","increase","decrease","decrease","no change","no change"))
#'
#' gg_corset_elongated(data = long.df, x_var = "time",
#'                    x_vals = c("pre","post"), y_var = "days",
#'                    group = "id", c_var = "change")
#'
#' ## Create groupings based on direction of change to use for eyelets:
#'
#' gg_corset_elongated(data = long.df, x_var = "time", x_vals = c("pre","post"),
#'                     y_var = "days", group = "id", c_var = "direction", eyelets = TRUE)
#'
#' ## Create faceted corset plots based on direction of change:
#'
#' gg_corset_elongated(data = long.df, x_var = "time", x_vals = c("pre","post"),
#'                     y_var = "days", group = "id", c_var = "direction", faceted = TRUE)
#'
#' @importFrom dplyr %>% filter summarize group_by select
#' @importFrom ggplot2 ggplot geom_point geom_pointrange geom_line aes position_dodge position_nudge theme_classic facet_wrap
#' @importFrom gghalves geom_half_violin
#' @importFrom ggstance position_dodgev
#' @importFrom stats reshape var
#' @export

globalVariables(c("mean_y","se_y"))

## FOR LONG-FORM DATA
gg_corset_elongated <- function(data, x_var, x_vals, y_var, group, c_var, eyelets = FALSE, faceted = FALSE, vio_fill = NA, line_size = NA) {

  data <- as.data.frame(data)
  data$x_var <- data[,x_var]
  data$x_var <- factor(data$x_var, levels = c(x_vals))
  data$y_var <- data[,y_var]
  data$group <- data[,group]
  data$c_var <- data[,c_var]

  vio_fill <- ifelse(is.na(vio_fill),"#0F0F0F",vio_fill)
  line_size <- ifelse(is.na(line_size),0.25,line_size)

  ## Basic Corset Plot
  if(eyelets == F & faceted == F) {

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

    geom_line(mapping = aes(group = group, colour = c_var),
              position = ggstance::position_dodgev(height = 0.1),
              size = line_size, alpha = 1) +

    gghalves::geom_half_violin(
      data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
      position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

    gghalves::geom_half_violin(
      data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
      position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

    theme_classic()

  }

  ## Corset Plot with Eyelets
  if(eyelets == T & faceted == F) {

    # calculating standard error means (SEM)
    data.summ <- data %>% group_by(c_var, x_var) %>%
      summarize(mean_y = mean(y_var, na.rm = T),
                se_y = sqrt(var(y_var,na.rm = T)/length(y_var)))

    data.summ$se_y <- ifelse(data.summ$se_y<min(data$y_var, na.rm = T),min(data$y_var, na.rm = T),
                             ifelse(data.summ$se_y>max(data$y_var, na.rm = T), max(data$y_var),
                                    data.summ$se_y))

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      gghalves::geom_half_violin(
        data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      geom_pointrange(
        data = data.summ %>% filter(x_var == x_vals[1]),
        mapping = aes(x = x_var, y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = -0.05, y = 0), size = 1, show.legend = F) +

      geom_pointrange(
        data = data.summ %>% filter(x_var == x_vals[2]),
        mapping = aes(x = x_var, y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = 0.05, y = 0), size = 1, show.legend = F) +

      theme_classic()
  }

  ## Faceted Corset Plot
  if(eyelets == F & faceted == T) {

    data2 <- dplyr::select(data, -c_var)

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

      geom_line(data = data2, mapping = aes(group = group), colour = "#B3B3B3",
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      gghalves::geom_half_violin(
        data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      theme_classic() + facet_wrap(~c_var)
  }

  ## Faceted Corset Plot with Eyelets
  if(eyelets == T & faceted == T) {

    # calculating standard error means (SEM)
    data.summ <- data %>% group_by(c_var, x_var) %>%
    summarize(mean_y = mean(y_var, na.rm = T),
              se_y = sqrt(var(y_var,na.rm = T)/length(y_var)))

    data.summ$se_y <- ifelse(data.summ$se_y<min(data$y_var, na.rm = T),min(data$y_var, na.rm = T),
                           ifelse(data.summ$se_y>max(data$y_var, na.rm = T), max(data$y_var),
                                  data.summ$se_y))

    data2 <- dplyr::select(data, -c_var)

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

      gghalves::geom_half_violin(
        data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      geom_line(data = data2, mapping = aes(group = group), colour = "#B3B3B3",
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      geom_pointrange(
        data = data.summ %>% filter(x_var == x_vals[1]),
        mapping = aes(x = x_var, y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = -0.075, y = 0), size = 1, show.legend = F) +

      geom_pointrange(
        data = data.summ %>% filter(x_var == x_vals[2]),
        mapping = aes(x = x_var, y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = 0.075, y = 0), size = 1, show.legend = F) +

      theme_classic() + facet_wrap(~c_var)

  }

  return(corset_plot)

}
