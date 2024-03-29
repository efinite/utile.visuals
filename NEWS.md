# utile.visuals 0.3.4

### Fixes & Minor Changes

* Replaced `dplyr` dependency with `vctrs`.

* Removed the `broom` package from suggests and from example documentation.

* Removed the `extract.legend = ` argument from the `append_table()` function. Legend extraction is now automatically performed if one is present.

* `append_table()` now vertically offsets the legend box instead of the legend itself. This fixes a spacing issue when multiple legends are present.

# utile.visuals 0.3.3

### New Features

* `panel_border()` function introduced to replace axis lines with a bordered panel.
Created for use with faceted plots, but can also be used standalone.

### Fixes & Minor Changes

* Added pkgdown site.

* Tweaks to the justification of axis text and scaling of line widths in `theme_basic()`
& `theme_risk()`.

* Theme facet label margins have been fixed and font size now matches that of axis
titles.

* Axis ticks in `theme_basic()` now scale appropriately with `base_size`.


# utile.visuals 0.3.2

### Fixes & Minor Changes
* Support for `linewidth` parameter introduced in `ggplot::` version 0.3.4.

* `theme_basic()` now has more consistent legend title formatting.

* Fix for a class check error in `ggrisktable()`.


# utile.visuals 0.3.1

* Consolidation of themes into `theme_basic()` and `theme_risk()`. Line and text color can now be selected with the `base_color` parameter. Additionally, sizes of elements and text now scale correctly with the specified `base_size` parameter.

* Fix for `survfit0` class check issues in `ggrisktable()`.


# utile.visuals 0.3.0

* `connect_origin()` was removed as it redundantly replicated the function of `survival::survfit0()`. Thanks for the tip, Beth!


# utile.visuals 0.2.4

* `connect_origin()` now verifies whether a strata column is present.


# utile.visuals 0.2.3

* Added parameter to `ggrisktable()` to allow preemptive specification of text color.


# utile.visuals 0.2.2

* Fixed inconsistency within `theme_white()`.


# utile.visuals 0.2.1

* `append_table()`: Legend extraction can now be toggled with the new `extract.legend` option. Extraction may not be a desired behavior if the legend is already embedded within plot area.

* `ggrisktable()`:
  - New option `strata.order` added to allow reordering of strata in final table.
  - Now includes hard stops for invalid option data.

* Added `connect_origin` for connecting KM curves to a plot origin.


# utile.visuals 0.2.0

* Added `ggrisktable()` for creating a ggplot2 risk table from a survival::survfit() model.

* Added `theme_risk_black()` and `theme_risk_white()` for risk table formatting.

* Added `append_table()` for adding a ggplot2 risk table to a ggplot2 Kaplan Meier curve.

* `theme_black()`|`theme_white()`: Updated axis title formating.

* Updated documentation.


# utile.visuals 0.1.1

* First public release
