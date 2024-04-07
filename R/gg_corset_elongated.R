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
#' @param line_size Optional. Use to change the size (thickness) of the lines which visualize change for each unit identified by the group variable. Default is 0.25.
#' @param line_col Optional custom colour of the background individual lines when the facet_design is set to "line". Defaults to a soft grey.
#' @param line_dodge Optional. Use to change the amount of vertical dodge of the lines which visualize each unit of the group variable. Default is 0.1.
#' @return ggplot2 graphical object
#' @examples
#'
#' long.df <- data.frame(id = c(rep(1:20,2)),
#'              time = c(rep(c("pre","post"),each = 20)),
#'              days  = c(3,4,7,5,6,3,4,1,7,0,5,2,0,1,6,2,1,7,4,6,
#'                        5,5,7,3,0,3,3,2,7,0,3,4,3,3,7,0,0,6,5,6),
#'              change = c(2,1,0,-2,-6,0,-1,1,0,0,-2,2,3,2,1,-2,-1,-1,1,0,
#'                         2,1,0,-2,-6,0,-1,1,0,0,-2,2,3,2,1,-2,-1,-1,1,0))
#'
#' long.df$direction <- ifelse(long.df$change==0,"No Change",
#'                            ifelse(long.df$change>0,"Increase","Decrease"))
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
#' @importFrom ggplot2 ggplot geom_point geom_pointrange geom_errorbar geom_line aes position_dodge position_nudge theme_classic facet_wrap
#' @importFrom gghalves geom_half_violin
#' @importFrom stats reshape var sd
#' @export

globalVariables(c("mean_y","sd_se","sd_se_min","sd_se_max"))

## FOR LONG-FORM DATA
gg_corset_elongated <- function(data, x_var, x_vals, y_var, group, c_var, eyelets = FALSE, e_type = "SE", faceted = FALSE, facet_design = "original", vio_fill = NA, line_size = NA, line_col = NA, line_dodge = 0.1) {

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
  if(eyelets == FALSE & faceted == FALSE) {

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = position_dodgev(height = line_dodge),
                linewidth = line_size, alpha = 1) +

      gghalves::geom_half_violin(
        data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

      gghalves::geom_half_violin(
        data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

      theme_classic()

  }

  ### Corset Plot with Eyelets
  if(eyelets == TRUE & faceted == FALSE) {

    if(e_type=="SE") {
      ## calculating standard error of the mean
      dat_mean <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) mean(x, na.rm = TRUE))
      dat_se <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) se(x))

      data.summ <- merge(dat_mean,dat_se, by = c("c_var","x_var"), all = TRUE)
      names(data.summ) <- c("c_var","x_var","mean_y","sd_se")

    }

    if(e_type=="SD"){
      ## calculating 1 standard deviation
      dat_mean <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) mean(x, na.rm = TRUE))
      dat_sd <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) stats::sd(x, na.rm = TRUE))

      data.summ <- merge(dat_mean,dat_sd, by = c("c_var","x_var"), all = TRUE)
      names(data.summ) <- c("c_var","x_var","mean_y","sd_se")
    }

    ## change to 0 if only 1 observation is present, as NAs will be produced for SE/ SD
    data.summ$sd_se <- ifelse(is.na(data.summ$sd_se),0, data.summ$sd_se)

    ## change SD/SE to not go beyond data limits (min/ max values) as this is not informative
    ## if mean - sd/se < min, sd/se == mean - min; if mean + sd/se > max, sd/se == max - mean
    data.summ$sd_se_min <- ifelse((data.summ$mean_y-data.summ$sd_se)<min(data$y_var, na.rm = TRUE),
                                  data.summ$mean_y-min(data$y_var, na.rm = TRUE), data.summ$sd_se)

    data.summ$sd_se_max <- ifelse((data.summ$mean_y+data.summ$sd_se)>max(data$y_var, na.rm = TRUE),
                                  max(data$y_var, na.rm = TRUE)-data.summ$mean_y,data.summ$sd_se)

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = position_dodgev(height = line_dodge),
                linewidth = line_size, alpha = 1) +

      gghalves::geom_half_violin(
        data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

      gghalves::geom_half_violin(
        data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

      theme_classic()

    if(e_type=="SE"){

      corset_plot <- corset_plot +

        geom_pointrange(
          data = data.summ[(data.summ$x_var==x_vals[1]),],
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.05, y = 0), size = 1, show.legend = FALSE) +

        geom_pointrange(
          data = data.summ[(data.summ$x_var==x_vals[2]),],
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.05, y = 0), size = 1, show.legend = FALSE)

    }

    if(e_type=="SD"){

      corset_plot <- corset_plot +

        geom_errorbar(
          data = data.summ[(data.summ$x_var==x_vals[1]),],
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.04, y = 0), width = 0.025, size = 1, show.legend = FALSE) +

        geom_errorbar(
          data = data.summ[(data.summ$x_var==x_vals[2]),],
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.04, y = 0), width = 0.025, size = 1, show.legend = FALSE) +

        geom_point(
          data = data.summ[(data.summ$x_var==x_vals[1]),],
          aes(x = x_var, y = mean_y, colour = c_var),
          position = position_nudge(x = -0.04, y = 0), size = 2.5, show.legend = FALSE) +

        geom_point(
          data = data.summ[(data.summ$x_var==x_vals[2]),],
          aes(x = x_var, y = mean_y, colour = c_var),
          position = position_nudge(x = 0.04, y = 0), size = 2.5, show.legend = FALSE)

    }

  }

  ### Faceted Corset Plot
  if(eyelets == F & faceted == T) {

    if(facet_design == "original"){

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = position_dodgev(height = line_dodge),
                  linewidth = line_size, alpha = 1) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "group"){

      data2 <- data[, -which(names(data) %in% c("c_var"))]

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = position_dodgev(height = line_dodge),
                  size = line_size, alpha = 1) +

        gghalves::geom_half_violin(
          data = data2[(data2$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data2[(data2$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 0.6, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 0.6, show.legend = FALSE) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "line"){

    data2 <- data[, -which(names(data) %in% c("c_var"))]

    corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

      geom_line(data = data2, mapping = aes(group = group), colour = line_col,
                position = position_dodgev(height = line_dodge),
                linewidth = line_size, alpha = 1) +

      geom_line(mapping = aes(group = group, colour = c_var),
                position = position_dodgev(height = line_dodge),
                linewidth = line_size, alpha = 1) +

      gghalves::geom_half_violin(
        data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

      gghalves::geom_half_violin(
        data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
        position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

      theme_classic() + facet_wrap(~c_var)

    }

  }

  ### Faceted Corset Plot with Eyelets
  if(eyelets == TRUE & faceted == TRUE) {

    if(e_type=="SE") {
      ## calculating standard error of the mean
      dat_mean <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) mean(x, na.rm = TRUE))
      dat_se <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) se(x))

      data.summ <- merge(dat_mean,dat_se, by = c("c_var","x_var"), all = TRUE)
      names(data.summ) <- c("c_var","x_var","mean_y","sd_se")
    }

    if(e_type=="SD"){
      ## calculating 1 standard deviation
      dat_mean <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) mean(x, na.rm = TRUE))
      dat_sd <- stats::aggregate(data[c("y_var")], by = list(c_var = data[,"c_var"], x_var = data[,"x_var"]), function(x) stats::sd(x, na.rm = TRUE))

      data.summ <- merge(dat_mean,dat_sd, by = c("c_var","x_var"), all = TRUE)
      names(data.summ) <- c("c_var","x_var","mean_y","sd_se")
    }

    ## change to 0 if only 1 observation is present, as NAs will be produced for SE/ SD
    data.summ$sd_se <- ifelse(is.na(data.summ$sd_se),0, data.summ$sd_se)

    ## change SD/SE to not go beyond data limits (min/ max values) as this is not informative
    ## if mean - sd/se < min, sd/se == mean - min; if mean + sd/se > max, sd/se == max - mean
    data.summ$sd_se_min <- ifelse((data.summ$mean_y-data.summ$sd_se)<min(data$y_var, na.rm = TRUE),
                                  data.summ$mean_y-min(data$y_var, na.rm = TRUE), data.summ$sd_se)

    data.summ$sd_se_max <- ifelse((data.summ$mean_y+data.summ$sd_se)>max(data$y_var, na.rm = TRUE),
                                  max(data$y_var, na.rm = TRUE)-data.summ$mean_y,data.summ$sd_se)


    if(facet_design == "original"){

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = position_dodgev(height = line_dodge),
                  linewidth = line_size, alpha = 1) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "group"){

      data2 <- data[, -which(names(data) %in% c("c_var"))]

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        gghalves::geom_half_violin(
          data = data2[(data2$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data2[(data2$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 0.6, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var, group = c_var, fill = c_var),
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 0.6, show.legend = FALSE) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = position_dodgev(height = line_dodge),
                  size = line_size, alpha = 1) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(facet_design == "line"){

      data2 <-  data[, -which(names(data) %in% c("c_var"))]

      corset_plot <- ggplot(data = data, aes(x = x_var, y = y_var)) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[1]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = -0.01,y = 0), size = 0, side = "l", alpha = 1, show.legend = FALSE) +

        gghalves::geom_half_violin(
          data = data[(data$x_var==x_vals[2]),], mapping = aes(x = x_var, y = y_var), fill = vio_fill,
          position = position_nudge(x = 0.01,y = 0), size = 0, side = "r", alpha = 1, show.legend = FALSE) +

        geom_line(data = data2, mapping = aes(group = group), colour = line_col,
                  position = position_dodgev(height = line_dodge),
                  size = line_size, alpha = 1) +

        geom_line(mapping = aes(group = group, colour = c_var),
                  position = position_dodgev(height = line_dodge),
                  size = line_size, alpha = 1) +

        theme_classic() + facet_wrap(~c_var)

    }

    if(e_type=="SE"){

      corset_plot <- corset_plot +

        geom_pointrange(
          data = data.summ[(data.summ$x_var==x_vals[1]),],
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.075, y = 0), size = 1, show.legend = FALSE) +

        geom_pointrange(
          data = data.summ[(data.summ$x_var==x_vals[2]),],
          mapping = aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.075, y = 0), size = 1, show.legend = FALSE)

    }

    if(e_type=="SD"){

      corset_plot <- corset_plot +

        geom_errorbar(
          data = data.summ[(data.summ$x_var==x_vals[1]),],
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = -0.06, y = 0), width = 0.025, size = 1, show.legend = FALSE) +

        geom_errorbar(
          data = data.summ[(data.summ$x_var==x_vals[2]),],
          aes(x = x_var, y = mean_y, ymin = mean_y - sd_se_min, ymax = mean_y + sd_se_max, colour = c_var),
          position = position_nudge(x = 0.06, y = 0), width = 0.025, size = 1, show.legend = FALSE) +

        geom_point(data = data.summ[(data.summ$x_var==x_vals[1]),],
                   aes(x = x_var, y = mean_y, colour = c_var),
                   position = position_nudge(x = -0.06, y = 0), size = 2.5, show.legend = FALSE) +

        geom_point(data = data.summ[(data.summ$x_var==x_vals[2]),],
                   aes(x = x_var, y = mean_y, colour = c_var),
                   position = position_nudge(x = 0.06, y = 0), size = 2.5, show.legend = FALSE)

    }

  }

  return(corset_plot)

}
