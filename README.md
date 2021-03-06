# utile.visuals
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/utile.visuals)](https://CRAN.R-project.org/package=utile.visuals)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/utile.visuals)](https://CRAN.R-project.org/package=utile.visuals)

## Overview
A small set of functions for making visuals with ggplot2. Includes minimalist themes with transparent backgrounds and a suite of tools for creating survival curves with risk tables.

## Functions
### > gg
- `ggrisktable()`: A simple wrapper function which calculates the numbers at risk for a survival model and a given set of time points then creates a ggplot2 table with them.

### > geom_
- `geom_stepconfint()`: Produces a step function confidence interval for survival curves. Essentially the `ggplot2::geom_step()` for confidence intervals which ggplot2 elects not to provide.

### > theme_
- `theme_white()`: A ggplot2 theme which removes most background elements and makes all text/lines white.
- `theme_black()`: A ggplot2 theme which removes most background elements and makes all text/lines black.
- `theme_risk_white()`: Minimalist ggplot2 theme which removes most background elements and makes all text/lines white.
- `theme_risk_black()`: Minimalist ggplot2 theme which removes most background elements and makes all text/lines black.

### > append_
- `append_table()`: Aligns axes and combines a ggplot2 plot and table into a single plot. Can handle legends.
