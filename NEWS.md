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
