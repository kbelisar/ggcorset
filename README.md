# The {ggcorset} package

The {ggcorset} package introduces a new visualization technique coined the 'corset plot'. It is used strictly to visualize discrete repeat measures data at 2 time points (such as pre- and post- data). The distribution of measurements at each time point is visualized using a half violin. Additionally, the trajectory of individual change is visualized by connecting these two points linearly, which can be filled to visualize the magnitude of change or other user-defined observed value. This method of visualization is ideal for showing the true heterogeneity of data.

The package relies on {ggplot2} to produce the visualizations. As such, the corset plot allows for easy integration with {ggplot2}, so that users can customize their visualizations as required. This package is geared towards users with limited experience in R, creating corset plots using data in either wide or long format using the functions `gg_corset()` or `gg_corset_elongated()`, respectively.

## Installation

To install the {ggcorset} package, use the {devtools} package to download directly this from GitHub repository:

```
devtools::install_github("kbelisar/ggcorset")
```

## gg_corset()

This function is used to create a corset plot with data in the wide format. It takes the following arguments:

`data` = the name of the data frame

`y_var1` = the name of the measured variable at the first time point

`y_var2` = the name of the measured variable at the second time point

`group` = the name of the units measured at each time point (such as id)

`c_var` = the name of the variable to visualize by line colour


## gg_corset_elongated()

This function is used to create a corset plot with data in the long format. It takes the following arguments:

`data` = the name of the data frame

`x_var` = the name of the x-axis variable (time variable)

`x_vals` = the values representing the two time points (in chronological order)

`y_var` = the name of the repeated measure

`group` = the name of the units measured at each time point (such as id)

`c_var` = the name of the variable to visualize by line colour


## A Quick Guide on {ggplot2} Customization:

By creating a corset plot as an R object, {ggplot2} customizations can easily be made. Here are a few changes to get started:

`xlab()` - Change the x-axis title

`ylab()` - Change the y-axis title

`ggtitle()` - Title of the plot

`scale_x_discrete()` - Change the labels of time1 and time2 (particularly helpful when using the `gg_corset` function)

`scale_colour_viridis()` - From the {viridis} package, it provides colour-blind friendly colours for the c_var variable


## Full Example

The example data set included in this package (named 'drinkdays') is in wide format, and has been simulated with mock data. This data set consists of 300 individuals, with 1 individual per row, as identified by a distinct 'id'. The repreated measure is the number of drinking days per week, measured at 'time1' and 'time2'. The variable 'change', which will be the `c_var`, is calculated by subtracting 'time1' from 'time2':

```
drinkdays$change <- drinkdays$time2-drinkdays$time1
```

**Since this data is in wide format, the function `gg_corset()` will be used to create the corset plot using the following code:**

```
p1 <- gg_corset(drinkdays, y_var1 = "pre", y_var2 = "post", group. = "id", c_var = "change")

p1    # to see the initial visualization
```

**Changes to the corset plot can be made by using {ggplot2}:**
```
library(ggplot2)

p1 <- p1 + xlab("Time") + ylab("Mean Days per Week") +      # change x and y axis labels
      ggtitle("Change in Mean Drinking Days per Week") +    # add a plot title
      scale_x_discrete(labels = c("Pre","Post")) +          # rename the time1 and time2 values on the x-axis
      scale_colour_viridis(option = "mako", 
                           breaks = c(-7,0,7),              # can set the legend limits here
                           name = "Change in Days")         # can rename the legend title here
                           
p1    # to see the modified plot
```
