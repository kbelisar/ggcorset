![CRAN](https://www.r-pkg.org/badges/version/ggcorset)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggcorset?color=yellow)](https://r-pkg.org/pkg/ggcorset)
![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/ggcorset?color=lightgrey)
[![R-CMD-check](https://github.com/kbelisar/ggcorset/actions/workflows/r_cmd_check.yml/badge.svg)](https://github.com/kbelisar/ggcorset/actions/workflows/r_cmd_check.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4905031.svg)](https://doi.org/10.5281/zenodo.4905031)

<img align="right" width="600" height="400" src="https://github.com/kbelisar/ggcorset/blob/main/visualizations/example_corset_plot_github_categorical.png">

# The 'ggcorset' package

The 'ggcorset' package introduces a visualization technique coined the corset plot. It is used strictly to visualize repeat measures data at 2 time points (such as pre- and post- data). The distribution of measurements at each time point is visualized using a half violin, whilst the trajectories of individual change are visualized via spaghetti plots by connecting these two points linearly. These lines can be filled to visualize the magnitude of change or other user-defined observed value. This method of visualization is ideal for showing the true heterogeneity of data, including differences by sub-groups.

The package relies on the '[ggplot2](https://github.com/tidyverse/ggplot2])' package to produce the visualizations. As such, the corset plot allows for easy integration with 'ggplot2', so that users can customize their visualizations as required. This package is geared towards users with limited experience in R, creating corset plots using data in either wide or long format using the functions `gg_corset()` or `gg_corset_elongated()`, respectively.

## New Features for v 0.2.0

Addition of optional 'eyelets' which visualize the standard error mean of user-defined groups (as seen in the figure above); optional faceting to better compare groups (figure below); as well as an added ggplot2 theme to quickly polish the visualization.

<p align="center">
<img width="600" height="400" src="https://github.com/kbelisar/ggcorset/blob/main/visualizations/example_corset_plot_github_faceted.png">
</p>

Additionally, the GitHub version includes the addition of an eyelet type which visualizes 1 standard deviation above and below the mean (using the optional e_type = "SE" argument). Note this feature is most interpretable when using the faceted option.

<p align="center">
<img width="600" height="400" src="https://github.com/kbelisar/ggcorset/blob/main/visualizations/example_corset_plot_github_sd_eyelets_age.png">
</p>


## Installation

Install the package directly from CRAN using:

```
install.packages("ggcorset")
```

The latest version of this package can also be installed using install_github() from the 'devtools' package to download from this GitHub respository directly:

```
devtools::install_github("kbelisar/ggcorset")
```

## Corset Plots in Action

An example of corset plots in use, can be seen in the following publication: 

> Minhas, M., Belisario, K., González-Roz, A., Halladay, J., Murphy, J.G. and MacKillop, J. (2021), COVID-19 impacts on drinking and mental health in emerging adults: Longitudinal changes and moderation by economic disruption and sex. Alcohol Clin Exp Res. https://doi.org/10.1111/acer.14624

> More research to come!


## The ggcorset functions

### gg_corset()

This function is used to create a corset plot with data in the wide format. It takes the following arguments:

`data` = the name of the data frame

`y_var1` = the name of the measured variable at the first time point

`y_var2` = the name of the measured variable at the second time point

`group` = the name of the units measured at each time point (such as id)

`c_var` = the name of the variable to visualize by line colour

`eyelets` = optional argument (defaults to FALSE) which creates either standard error means for the c_var (default) or 1 standard deviation above/ below the mean as defined by the 'e_type` argument

`e_type` = optional argument when `eyelets` is set to TRUE. One of "SE" (default) or "SD" [[Currently Available for the GitHub version only]]

`faceted` = optional argument (defaults to FALSE) which facets corset plots by c_var with soft grey lines denoting entire sample trajectories

`vio_fill` = optional argument to change the fill colour of the half violins (defaults to a soft black)

`line_size` = optional argument to change the size (thickness) of the lines (default is 0.25)


### gg_corset_elongated()

This function is used to create a corset plot with data in the long format. It takes the following arguments:

`data` = the name of the data frame

`x_var` = the name of the x-axis variable (time variable)

`x_vals` = the values representing the two time points (in chronological order)

`y_var` = the name of the repeated measure

`group` = the name of the units measured at each time point (such as id)

`c_var` = the name of the variable to visualize by line colour

`eyelets` = optional argument (defaults to FALSE) which creates standard error means for the c_var (default) or 1 standard deviation above/ below the mean as defined by the 'e_type` argument

`e_type` = optional argument when `eyelets` is set to TRUE. One of "SE" (default) or "SD" [[Currently Available for the GitHub version only]]

`faceted` = optional argument (defaults to FALSE) which facets corset plots by c_var with soft grey lines denoting entire sample trajectories

`vio_fill` = optional argument to change the fill colour of the half violins (defaults to a soft black)

`line_size` = optional argument to change the size (thickness) of the lines (default is 0.25)

### theme_ggcorset()

This is a 'ggplot2' theme to quickly polish visualizations, and can be added to any ggplot2 object.


## A Quick Guide on 'ggplot2' Customization:

By creating a corset plot as an R object, 'ggplot2' customizations can easily be made. Here are a few changes to get started:

`xlab()` - Change the x-axis title

`ylab()` - Change the y-axis title

`ggtitle()` - Title of the plot

`scale_x_discrete()` - Change the labels of time1 and time2 (particularly helpful when using the `gg_corset()` function)

`scale_colour_manual()` - Custom colours as specified by the user (via `values =` argument)

`scale_colour_gradientn()` - Custom gradient of colours for continuous outcomes

`scale_colour_viridis()` - Custom colour-blind friendly colours for the c_var variable using the 'viridis' package

`guides(colour = guide_legend(override.aes = list(size = 3)))` - Overrides thickness of legend so colours are more easily seen


## Full Example

The example data set included in this package (named 'drinkdays') is in wide format, and has been simulated with mock data. This data set consists of 300 individuals, with 1 individual per row, as identified by a distinct 'id'. The repreated measure is the number of drinking days per week, measured at 'time1' and 'time2'.

The variable 'change', is calculated by subtracting 'time1' from 'time2', and is used for the `c_var` argument:

```
library(ggcorset)

data("drinkdays")

#Calculating Change
drinkdays$change <- drinkdays$time2-drinkdays$time1

```

**Since this data is in wide format**, the function `gg_corset()` will be used to create the corset plot using the following code:

```
plot <- gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2", 
                  group = "id", c_var = "change", eyelets = F) +
        theme_ggcorset()                                           # A ggcorset theme for polished corset plots!

plot # see the initial visualization
```

**Changes to the corset plot can be made by using 'ggplot2':**
```
library(ggplot2)

plot + 
  scale_colour_gradientn("Direction of Change",
                         colours = MetBrewer::met.brewer("Troy")) +     # Changes legend title, and selects a colour-palette
  ggtitle("Change in Drinking Days") +                                  # Changes the plot title
  ylab("Number of Drinking Days per Week") +                            # Changes the y-axis title
  xlab("") +                                                            # Changes the x-axis title (removes in favour of the 2 time point labels below)
  scale_x_discrete(labels = c("Pre","Post")) +                          # Changes the labels of the 2 time points (on the x-axis)
  guides(colour = guide_legend(override.aes = list(size = 3)))          # Makes the legend lines thicker

```
**Example Corset Plot:**

This plot was solely created using the example data set and the code used in the example above:

<p align="center">
<img width="600" height="400" src="https://github.com/kbelisar/ggcorset/blob/main/visualizations/example_corset_plot_github_continuous.png">
</p>
