### The following code for vertical dodging in ggplot (position_dodgev) is adapted from Lionel Henry's ggstance package
### as it will be archived on CRAN in 2024 [https://github.com/lionel-/ggstance]
### Edits include removing unused arguments; combining functions; and removing the use of the plyr::ddply function

pos_dodgev <- function(df, height) {

  n <- length(unique(df$group))

  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }

  d_height <- max(df$ymax - df$ymin)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidy <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate ymin and ymax
  df$y <- df$y + height * ((groupidy - 0.5) / n - .5)
  df$ymin <- df$y - d_height / n / 2
  df$ymax <- df$y + d_height / n / 2

  return(df)
}

#' @inheritParams ggplot2::position_dodge

collidev <- function(data, height = NULL, name, strategy) {

  # Determine height
  if (!is.null(height)) {
    # Width set manually
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y - height / 2
      data$ymax <- data$y + height / 2
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }

    # Width determined from data, must be floating point constant
    heights <- unique(data$ymax - data$ymin)
    heights <- heights[!is.na(heights)]
    height <- heights[1]
  }

  # Reorder by x position, then on group
  data <- data[order(data$ymin, data$group), ]

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]

  if (!is.null(data$xmax)) {
    data <- pos_dodgev(data,height)
  } else if (!is.null(data$x)) {
    data$xmax <- data$x
    data <- pos_dodgev(data,height)
    data$x <- data$xmax
    return(data)
  } else {
    stop("Neither x nor xmax defined")
  }
}

#' @format NULL
#' @usage NULL

PositionDodgev <- ggplot2::ggproto("PositionDodgev", ggplot2::Position,
                          height = NULL, preserve = "total",
                          setup_params = function(self, data) {
                            if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
                              warning("Height not defined. Set with `position_dodge(height = ?)`",
                                      call. = FALSE)
                            }

                            if (identical(self$preserve, "total")) {
                              n <- NULL
                            } else {
                              panels <- unname(split(data, data$PANEL))
                              ns <- vapply(panels, function(panel) max(table(panel$ymin)), double(1))
                              n <- max(ns)
                            }

                            list(
                              height = self$height,
                              n = n
                            )
                          },

                          setup_data = function(self, data, params) {
                            if (!"y" %in% names(data) && all(c("ymin", "ymax") %in% names(data))) {
                              data$y <- (data$ymin + data$ymax) / 2
                            }
                            data
                          },

                          compute_panel = function(data, params, scales) {
                            collidev(
                              data,
                              params$height,
                              name = "position_dodgev")
                          }
)


#' POSITION DODGE-V
#' @name position_dodgev
#' @noRd

position_dodgev <- function(height = NULL) {
  ggplot2::ggproto(NULL, PositionDodgev,
          height = height
  )
}
