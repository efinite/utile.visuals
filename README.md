# utile.visuals
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/utile.visuals)](https://CRAN.R-project.org/package=utile.visuals)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/utile.visuals)](https://CRAN.R-project.org/package=utile.visuals)

## Overview
A set of themes and functions for making visuals with ggplot2.

### Survival Curves
- `ggrisktable()`: Creates a ggplot2 risk table for a `survival::survfit` object.
- `geom_stepconfint()`: Produces a step function confidence interval for plotted 
survival curves.

### Themes
- `theme_basic()`: A ggplot2 theme for plots which removes most background elements.
- `theme_risk()`: A ggplot2 theme for risk tables which removes most background elements.
- `panel_border()`: Replace the axes of a ggplot2 plot with a bordered panel. 

### Miscellaneous
- `append_table()`: Combines and aligns a ggplot2 plot and table into a single plot.
