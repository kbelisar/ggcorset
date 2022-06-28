#' CORSET PLOT
#'
#' This function visualizes a corset plot in wide format.
#' @name gg_corset
#' @param data The name of the data frame.
#' @param y_var1 The name of measured variable at time 1.
#' @param y_var2 The name of measured variable at time 2.
#' @param group The name of units measured at each time point such as 'ID'.
#' @param c_var The name of variable to visualize by line colour, such as percent change, magnitude of change, or direction of change.
#' @param eyelets Optional (default is FALSE). If set to true, this will visualize one of two mean types by c_var, as defined by the 'e_type' argument.
#' @param e_type Optional eyelet type if the eyelets parameter is set to TRUE. One of "SE" or "SD". The default is standard error ("SE") means. Alternatively, standard deviations ("SD") with means can be specified, which include horizontal lines to denote +1 and -1 standard deviation. Note that the visualization of standard deviations works best in tandem with the faceted option.
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
#' ## Create faceted corset plots with standard deviation eyelets:
#'
#' gg_corset(data = wide.df, y_var1 = "time1", y_var2 = "time2", group = "id",
#'           c_var = "direction", e_type = "SD", faceted = TRUE)
#'
#' @importFrom dplyr %>% filter summarize group_by select
#' @importFrom ggplot2 ggplot geom_point geom_pointrange geom_errorbar geom_line aes position_dodge position_nudge theme_classic facet_wrap
#' @importFrom gghalves geom_half_violin
#' @importFrom ggstance position_dodgev
#' @importFrom stats reshape var
#' @export

globalVariables(c("x_axis", "y","mean_y","sd_se","sd_se_min","sd_se_max"))

## FOR WIDE-FORM DATA
gg_corset <- function(data, y_var1, y_var2, group, c_var, eyelets = FALSE, e_type = "SE", faceted = FALSE, vio_fill = NA, line_size = NA) {

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

    if(e_type=="SE") {
      # calculating standard error of the mean
      data.summ <- data.long %>% group_by(c_var, x_axis) %>%
        summarize(mean_y = mean(y, na.rm = T),
                  sd_se = sqrt(var(y,na.rm = T)/length(y)))
    }

    if(e_type=="SD"){
      # calculating 1 standard deviation
      data.summ <- data.long %>% group_by(c_var, x_axis) %>%
        summarize(mean_y = mean(y, na.rm = T),
                  sd_se = sqrt(var(y,na.rm = T)))
    }

    # change to 0 if only 1 observation is present, as NAs will be produced for SE/ SD
    data.summ$sd_se <- ifelse(is.na(data.summ$sd_se),0, data.summ$sd_se)

    # change SD/SE to not go beyond data limits (min or max values) as this is not informative
    # if mean - sd/se < min, sd/se == mean - min; if mean + sd/se > max, sd/se == max - mean
    data.summ$sd_se_min <- ifelse((data.summ$mean_y-data.summ$sd_se)<min(data.long$y, na.rm = T),
                                  data.summ$mean_y-min(data.long$y, na.rm = T), data.summ$sd_se)

    data.summ$sd_se_max <- ifelse((data.summ$mean_y+data.summ$sd_se)>max(data.long$y, na.rm = T),
                                  max(data.long$y, na.rm = T)-data.summ$mean_y,data.summ$sd_se)

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

    if(e_type=="SE"){

      corset_plot <- corset_plot +

        geom_pointrange(
          data = data.summ %>% filter(x_axis == "var1"),
          aes(y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.05, y = 0), size = 1, show.legend = F) +

        geom_pointrange(
          data = data.summ %>% filter(x_axis == "var2"),
          aes(y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.05, y = 0), size = 1, show.legend = F)

    }

    if(e_type=="SD"){

      corset_plot <- corset_plot +

        geom_errorbar(
          data = data.summ %>% filter(x_axis == "var1"),
          aes(x = x_axis, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.04, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_errorbar(
          data = data.summ %>% filter(x_axis == "var2"),
          aes(x = x_axis, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.04, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_point(data = data.summ %>% filter(x_axis == "var1"),
                   aes(x = x_axis, y = mean_y, colour = c_var),
                   position = position_nudge(x = -0.04, y = 0), size = 2.5, show.legend = F) +

        geom_point(data = data.summ %>% filter(x_axis == "var2"),
                   aes(x = x_axis, y = mean_y, colour = c_var),
                   position = position_nudge(x = 0.04, y = 0), size = 2.5, show.legend = F)

    }
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

    if(e_type=="SE") {
      # calculating standard error means (SEM)
      data.summ <- data.long %>% group_by(c_var, x_axis) %>%
        summarize(mean_y = mean(y, na.rm = T),
                  sd_se = sqrt(var(y,na.rm = T)/length(y)))
    }

    if(e_type=="SD"){
      data.summ <- data.long %>% group_by(c_var, x_axis) %>%
        summarize(mean_y = mean(y, na.rm = T),
                  sd_se = sqrt(var(y,na.rm = T)))
    }

    # change to 0 if only 1 observation is present, as NAs will be produced for SEM/ SD
    data.summ$sd_se <- ifelse(is.na(data.summ$sd_se),0, data.summ$sd_se)

    # change SD/SE to not go beyond data limits (min or max values) as this is not informative
    # if mean - sd/se < min, sd/se == mean - min; if mean + sd/se > max, sd/se == max - mean
    data.summ$sd_se_min <- ifelse((data.summ$mean_y-data.summ$sd_se)<min(data.long$y, na.rm = T),
                                  data.summ$mean_y-min(data.long$y, na.rm = T), data.summ$sd_se)

    data.summ$sd_se_max <- ifelse((data.summ$mean_y+data.summ$sd_se)>max(data.long$y, na.rm = T),
                                  max(data.long$y, na.rm = T)-data.summ$mean_y,data.summ$sd_se)

    data.long2 <- dplyr::select(data.long, -c_var)

    corset_plot <- ggplot(data = data.long, aes(x = x_axis, y = y)) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var1"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

      gghalves::geom_half_violin(
        data = data.long %>% filter(x_axis == "var2"), mapping = aes(x = x_axis, y = y), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

      geom_line(data = data.long2, mapping = aes(group = group), colour = "#B3B3B3",
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1) +

      geom_line(aes(group = group, colour = c_var),
                position = ggstance::position_dodgev(height = 0.1),
                size = line_size, alpha = 1)  +

      theme_classic() + facet_wrap(~c_var)


    if(e_type=="SE"){

      corset_plot <- corset_plot +

        geom_pointrange(
          data = data.summ %>% filter(x_axis == "var1"),
          aes(x = x_axis, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.075, y = 0), size = 1, show.legend = F) +

        geom_pointrange(
          data = data.summ %>% filter(x_axis == "var2"),
          aes(x = x_axis, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.075, y = 0), size = 1, show.legend = F)

    }

    if(e_type=="SD"){

      corset_plot <- corset_plot +

        geom_errorbar(
          data = data.summ %>% filter(x_axis == "var1"),
          aes(x = x_axis, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.06, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_errorbar(
          data = data.summ %>% filter(x_axis == "var2"),
          aes(x = x_axis, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.06, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_point(data = data.summ %>% filter(x_axis == "var1"),
                   aes(x = x_axis, y = mean_y, colour = c_var),
                   position = position_nudge(x = -0.06, y = 0), size = 2.5, show.legend = F) +

        geom_point(data = data.summ %>% filter(x_axis == "var2"),
                   aes(x = x_axis, y = mean_y, colour = c_var),
                   position = position_nudge(x = 0.06, y = 0), size = 2.5, show.legend = F)

    }

  }

  return(corset_plot)

}
