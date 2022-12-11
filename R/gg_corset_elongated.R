#' CORSET PLOT ELONGATED
#'
#' This function visualizes a corset plot in long format.
#' @name gg_corset_elongated
#' @param data The name of the data frame.
#' @param x_var The name of the x_axis variable.
#' @param x_vals The values of the two time points.
#' @param y_var The repeated measure variable name.
#' @param group The name of units measured at each time point such as 'ID'. The trajectories of these units are visualized by the lines of the corset plot.
#' @param c_var The name of variable to visualize by line colour, such as percent change.
#' @param eyelets Optional (default is FALSE). If set to true, this will visualize one of two mean types by c_var, as defined by the 'e_type' argument.
#' @param e_type Optional eyelet type if the eyelets parameter is set to TRUE. One of "SE" or "SD". The default is standard error ("SE") means. Alternatively, standard deviations ("SD") with means can be specified, which include horizontal lines to denote +1 and -1 standard deviation. Note that the visualization of standard deviations works best in tandem with the faceted option.
#' @param faceted Optional (default is FALSE). If set to true, the c_var will be faceted.
#' @param facet_design Optional facet type when the faceted parameter is set to TRUE. One of "original","group", or "line". The default is "original", which provides facets void of any special features. The "group" option includes the overall distribution of the entire sample in the background of each facet (which defaults to the 'vio_fill' colour), alongside each distribution for each c_var group. The "line" option includes all individual trajectories in the background of each facet using a soft grey (default) or custom colour as chosen by 'line_col' argument.
#' @param vio_fill Optional (defaults to a soft black). Use to change the fill colour of the half violins.
#' @param line_size Optional. Use to change the size (thickness) of the lines which visualize the c_var. Default is 0.25.
#' @param line_col Optional custom colour of the background individual lines when the facet_design is set to "line". Defaults to a soft grey.
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
#' ## Create faceted corset plots with standard deviation eyelets:
#'
#' gg_corset_elongated(data = long.df, x_var = "time", x_vals = c("pre","post"),
#'                     y_var = "days", group = "id", c_var = "direction",
#'                     e_type = "SD", faceted = TRUE)
#'
#' @importFrom dplyr %>% filter summarize group_by select
#' @importFrom ggplot2 ggplot geom_point geom_pointrange geom_errorbar geom_line aes position_dodge position_nudge theme_classic facet_wrap
#' @importFrom gghalves geom_half_violin
#' @importFrom ggstance position_dodgev
#' @importFrom stats reshape var
#' @export

globalVariables(c("mean_y","sd_se","sd_se_min","sd_se_max"))

## FOR LONG-FORM DATA
gg_corset_elongated <- function(data, x_var, x_vals, y_var, group, c_var, eyelets = FALSE, e_type = "SE", faceted = FALSE, facet_design = "original", vio_fill = NA, line_size = NA, line_col = NA) {

  data <- as.data.frame(data)
  data$x_var <- data[,x_var]
  data$x_var <- factor(data$x_var, levels = c(x_vals))
  data$y_var <- data[,y_var]
  data$group <- data[,group]
  data$c_var <- data[,c_var]

  vio_fill <- ifelse(is.na(vio_fill),"#0F0F0F",vio_fill)
  line_size <- ifelse(is.na(line_size),0.25,line_size)
  line_col <- ifelse(is.na(line_col),"#B3B3B3",line_col)

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

    if(e_type=="SE") {
      # calculating standard error of the mean
      data.summ <- data %>% group_by(c_var, x_var) %>%
        summarize(mean_y = mean(y_var, na.rm = TRUE),
                  sd_se = sqrt(var(y_var[!is.na(y_var)])/length(y_var[!is.na(y_var)])))
    }

    if(e_type=="SD"){
      # calculating 1 standard deviation
      data.summ <- data %>% group_by(c_var, x_var) %>%
        summarize(mean_y = mean(y_var, na.rm = TRUE),
                  sd_se = sqrt(var(y_var,na.rm = T)))
    }

    # change to 0 if only 1 observation is present, as NAs will be produced for SE/ SD
    data.summ$sd_se <- ifelse(is.na(data.summ$sd_se),0, data.summ$sd_se)

    # change SD/SE to not go beyond data limits (min/ max values) as this is not informative
    # if mean - sd/se < min, sd/se == mean - min; if mean + sd/se > max, sd/se == max - mean
    data.summ$sd_se_min <- ifelse((data.summ$mean_y-data.summ$sd_se)<min(data$y_var, na.rm = T),
                                  data.summ$mean_y-min(data$y_var, na.rm = T), data.summ$sd_se)

    data.summ$sd_se_max <- ifelse((data.summ$mean_y+data.summ$sd_se)>max(data$y_var, na.rm = T),
                                  max(data$y_var, na.rm = T)-data.summ$mean_y,data.summ$sd_se)

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

    if(e_type=="SE"){

      corset_plot <- corset_plot +

        geom_pointrange(
          data = data.summ %>% filter(x_var == x_vals[1]),
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.05, y = 0), size = 1, show.legend = F) +

        geom_pointrange(
          data = data.summ %>% filter(x_var == x_vals[2]),
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.05, y = 0), size = 1, show.legend = F)

    }

    if(e_type=="SD"){

      corset_plot <- corset_plot +

        geom_errorbar(
          data = data.summ %>% filter(x_var == x_vals[1]),
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.04, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_errorbar(
          data = data.summ %>% filter(x_var == x_vals[2]),
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.04, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_point(
          data = data.summ %>% filter(x_var == x_vals[1]),
          aes(x = x_var, y = mean_y, colour = c_var),
          position = position_nudge(x = -0.04, y = 0), size = 2.5, show.legend = F) +

        geom_point(
          data = data.summ %>% filter(x_var == x_vals[2]),
          aes(x = x_var, y = mean_y, colour = c_var),
          position = position_nudge(x = 0.04, y = 0), size = 2.5, show.legend = F)

    }

  }

  ## Faceted Corset Plot
  if(eyelets == F & faceted == T) {

    if(facet_design == "original"){

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

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "group"){

      data2 <- dplyr::select(data, -c_var)

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = ggstance::position_dodgev(height = 0.1),
                  size = line_size, alpha = 1) +

        gghalves::geom_half_violin(
          data = data2 %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

        gghalves::geom_half_violin(
          data = data2 %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 0.6, show.legend = F) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 0.6, show.legend = F) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "line"){

    data2 <- dplyr::select(data, -c_var)

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

      geom_line(data = data2, mapping = aes(group = group), colour = line_col,
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

  }

  ## Faceted Corset Plot with Eyelets
  if(eyelets == T & faceted == T) {

    if(e_type=="SE") {
      # calculating standard error of the mean
      data.summ <- data %>% group_by(c_var, x_var) %>%
        summarize(mean_y = mean(y_var, na.rm = TRUE),
                  sd_se = sqrt(var(y_var[!is.na(y_var)])/length(y_var[!is.na(y_var)])))
    }

    if(e_type=="SD"){
      # calculating 1 standard deviation
      data.summ <- data %>% group_by(c_var, x_var) %>%
        summarize(mean_y = mean(y_var, na.rm = TRUE),
                  sd_se = sqrt(var(y_var,na.rm = T)))
    }

    # change to 0 if only 1 observation is present, as NAs will be produced for SE/ SD
    data.summ$sd_se <- ifelse(is.na(data.summ$sd_se),0, data.summ$sd_se)

    # change SD/SE to not go beyond data limits (min/ max values) as this is not informative
    # if mean - sd/se < min, sd/se == mean - min; if mean + sd/se > max, sd/se == max - mean
    data.summ$sd_se_min <- ifelse((data.summ$mean_y-data.summ$sd_se)<min(data$y_var, na.rm = T),
                                  data.summ$mean_y-min(data$y_var, na.rm = T), data.summ$sd_se)

    data.summ$sd_se_max <- ifelse((data.summ$mean_y+data.summ$sd_se)>max(data$y_var, na.rm = T),
                                  max(data$y_var, na.rm = T)-data.summ$mean_y,data.summ$sd_se)


    if(facet_design == "original"){

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = ggstance::position_dodgev(height = 0.1),
                  size = line_size, alpha = 1) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "group"){

      data2 <- dplyr::select(data, -c_var)

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        gghalves::geom_half_violin(
          data = data2 %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

        gghalves::geom_half_violin(
          data = data2 %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 0.6, show.legend = F) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 0.6, show.legend = F) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = ggstance::position_dodgev(height = 0.1),
                  size = line_size, alpha = 1) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "line"){

      data2 <- dplyr::select(data, -c_var)

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[1]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = F) +

        gghalves::geom_half_violin(
          data = data %>% filter(x_var == x_vals[2]), mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = F) +

        geom_line(data = data2, mapping = aes(group = group), colour = line_col,
                  position = ggstance::position_dodgev(height = 0.1),
                  size = line_size, alpha = 1) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = ggstance::position_dodgev(height = 0.1),
                  size = line_size, alpha = 1) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(e_type=="SE"){

      corset_plot <- corset_plot +

        geom_pointrange(
          data = data.summ %>% filter(x_var == x_vals[1]),
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.075, y = 0), size = 1, show.legend = F) +

        geom_pointrange(
          data = data.summ %>% filter(x_var == x_vals[2]),
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.075, y = 0), size = 1, show.legend = F)

    }

    if(e_type=="SD"){

      corset_plot <- corset_plot +

        geom_errorbar(
          data = data.summ %>% filter(x_var == x_vals[1]),
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.06, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_errorbar(
          data = data.summ %>% filter(x_var == x_vals[2]),
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.06, y = 0), width = 0.025, size = 1, show.legend = F) +

        geom_point(data = data.summ %>% filter(x_var == x_vals[1]),
                   aes(x = x_var, y = mean_y, colour = c_var),
                   position = position_nudge(x = -0.06, y = 0), size = 2.5, show.legend = F) +

        geom_point(data = data.summ %>% filter(x_var == x_vals[2]),
                   aes(x = x_var, y = mean_y, colour = c_var),
                   position = position_nudge(x = 0.06, y = 0), size = 2.5, show.legend = F)

    }

  }

  return(corset_plot)

}
