#' CORSET PLOT
#'
#' This function visualizes a corset plot in wide format.
#' @name gg_corset
#' @param data The name of the data frame.
#' @param y_var1 The name of measured variable at time 1.
#' @param y_var2 The name of measured variable at time 2.
#' @param group The name of units measured at each time point such as 'ID'.
#' @param c_var The name of variable to visualize by line colour, such as percent change, magnitude of change, or direction of change.
#' @param eyelets Optional (default is FALSE). If set to true, this will visualize standard error means (SEM) by c_var.
#' @param faceted Optional (default is FALSE). If set to true, the c_var will be faceted, with all lines visible in soft grey as a background in each facet.
#' @param vio_fill Optional (defaults to a soft black). Use to change the fill colour of the half violins.
#' @param line_size Optional. Use to change the size (thickness) of the lines which visualize the c_var. Default is 0.25.
#' @return ggplot2 graphical object
#' @examples
#'
#' wide.df <- data.frame(id = c(1,2,3,4,5),
#'              time1 = c(3,4,7,5,6),
#'              time2  = c(5,5,7,3,0),
#'              change = c(28.57,14.29,0,-28.57,-85.71),
#'              direction = c("increase","increase","no change","decrease","decrease"))
#'
#' gg_corset(data = wide.df, y_var1 = "time1", y_var2 = "time2",
#'           group = "id", c_var = "change")
#'
#' ## Create corset plots with eyelets:
#'
#' gg_corset(data = wide.df, y_var1 = "time1", y_var2 = "time2",
#'           group = "id", c_var = "direction", eyelets = TRUE)
#'
#' ## Create faceted corset plots based on direction of change:
#'
#' gg_corset(data = wide.df, y_var1 = "time1", y_var2 = "time2",
#'           group = "id", c_var = "direction", faceted = TRUE)
#'
#' @importFrom dplyr %>% filter summarize group_by select
#' @importFrom ggplot2 ggplot geom_point geom_pointrange geom_line aes position_dodge position_nudge theme_classic facet_wrap
#' @importFrom gghalves geom_half_violin
#' @importFrom ggstance position_dodgev
#' @importFrom stats reshape var
#' @export

globalVariables(c("x_axis", "y","mean_y","se_y"))

## FOR WIDE-FORM DATA
gg_corset <- function(data, y_var1, y_var2, group, c_var, eyelets = FALSE, faceted = FALSE, vio_fill = NA, line_size = NA) {

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
  line_size <- ifelse(is.na(line_size),0.25,line_size)

  ## Basic Corset Plot
  if(eyelets == F & faceted == F) {
    corset_plot <- ggplot(data = data.long, aes(x = x_axis, y = y)) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var1"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var2"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      theme_classic()

  }
  ## Corset Plot with Eyelets
  if(eyelets == T & faceted == F) {

    # calculating standard error means (SEM)
    data.summ <- data.long %>% group_by(c_var, x_axis) %>%
      summarize(mean_y = mean(y, na.rm = T),
                se_y = sqrt(var(y,na.rm = T)/length(y)))

    data.summ$se_y <- ifelse(data.summ$se_y<min(data.long$y, na.rm = T),min(data.long$y, na.rm = T),
                             ifelse(data.summ$se_y>max(data.long$y, na.rm = T), max(data.long$y),
                                    data.summ$se_y))

    corset_plot <- ggplot(data = data.long, aes(x = x_axis, y = y)) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var1"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var2"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      geom_pointrange(
        data = data.summ %>% filter(x_axis == "var1"),
        mapping = aes(x = x_axis, y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = -0.05, y = 0), size = 1, show.legend = F) +

      geom_pointrange(
        data = data.summ %>% filter(x_axis == "var2"),
        mapping = aes(x = x_axis, y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = 0.05, y = 0), size = 1, show.legend = F) +

      theme_classic()

  }
  ## Faceted Corset Plot
  if(eyelets == F & faceted == T) {

    data.long2 <- dplyr::select(data.long, -c_var)

    corset_plot <- ggplot(data = data.long, aes(x = x_axis, y = y)) +

      geom_line(data = data.long2, mapping = aes(group = group), colour = "#B3B3B3",
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      geom_line(aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1)  +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var1"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var2"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      theme_classic() + facet_wrap(~c_var)

  }
  ## Faceted Corset Plot with Eyelets
  if(eyelets == T & faceted == T) {

    # calculating standard error means (SEM)
    data.summ <- data.long %>% group_by(c_var, x_axis) %>%
      summarize(mean_y = mean(y, na.rm = T),
                se_y = sqrt(var(y,na.rm = T)/length(y)))

    data.summ$se_y <- ifelse(data.summ$se_y<min(data.long$y, na.rm = T),min(data.long$y, na.rm = T),
                             ifelse(data.summ$se_y>max(data.long$y, na.rm = T), max(data.long$y),
                                    data.summ$se_y))

    data.long2 <- dplyr::select(data.long, -c_var)

    corset_plot <- ggplot(data = data.long, aes(x = x_axis, y = y)) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var1"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var2"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      geom_pointrange(
        data = data.summ %>% filter(x_axis == "var1"),
        aes(y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = -0.075, y = 0), size = 1, show.legend = F) +

      geom_pointrange(
        data = data.summ %>% filter(x_axis == "var2"),
        aes(y = mean_y, ymin = mean_y - se_y, ymax = mean_y + se_y, colour = c_var),
        position = position_nudge(x = 0.075, y = 0), size = 1, show.legend = F) +

      geom_line(data = data.long2, mapping = aes(group = group), colour = "#B3B3B3",
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      geom_line(aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1)  +

      theme_classic() + facet_wrap(~c_var)

  }

  return(corset_plot)

}

