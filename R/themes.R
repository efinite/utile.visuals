#' @importFrom ggplot2 %+replace%

#' @title Minimalist theme for ggplot2
#' @description A minimalist \code{ggplot2} theme which removes most background elements and
#' lines.
#' @param base_size A numeric. Base font size.
#' @param base_family A numeric. Base font family.
#' @param base_color A character. Base color for lines and text.
#' @param base_line_size A numeric. Base line element size.
#' @param base_rect_size A numeric. Base rectangle element size.
#' @note Recommend exporting as PNG or TIFF to preserve
#' background transparency.
#' @examples
#' library(ggplot2)
#'
#' ggplot(datasets::mtcars, aes(x = wt, y = hp, color = as.factor(cyl))) +
#'   geom_point() +
#'   theme_basic()
#' @export
theme_basic <- function(
  base_size = 12,
  base_family = NULL,
  base_color = "black",
  base_line_size = base_size/12,
  base_rect_size = base_size/12
) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(

      # Defaults
      text = ggplot2::element_text(
        family = base_family,
        color = base_color,
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = ggplot2::margin(),
        debug = FALSE),
      line = ggplot2::element_line(
        color = base_color,
        linewidth = base_line_size,
        linetype = 1,
        lineend = "square"),
      rect = ggplot2::element_rect(
        fill = "transparent",
        colour = base_color,
        linewidth = base_rect_size,
        linetype = 1),

      # Axis
      axis.line = ggplot2::element_line(
        colour = base_color, linewidth = base_line_size),
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.ticks = ggplot2::element_line(color = base_color, linewidth = base_line_size),
      axis.ticks.length = ggplot2::unit(base_size / 2.5, "pt"),
      axis.ticks.length.x = NULL,
      axis.ticks.length.x.top = NULL,
      axis.ticks.length.x.bottom = NULL,
      axis.ticks.length.y = NULL,
      axis.ticks.length.y.left = NULL,
      axis.ticks.length.y.right = NULL,
      axis.title = ggplot2::element_text(color = base_color),
      axis.title.x = ggplot2::element_text(
        angle = 0, margin = ggplot2::margin(t = base_size * 0.6), vjust = 1),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(b = base_size * 0.6), vjust = 0),
      axis.title.y = ggplot2::element_text(
        angle = 90, margin = ggplot2::margin(r = base_size * 0.6), vjust = 1),
      axis.title.y.right = ggplot2::element_text(
        angle = -90, margin = ggplot2::margin(l = base_size * 0.6), vjust = 0),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.95), color = base_color),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 0.8 * base_size / 4), vjust = 0.25),
      axis.text.x.top = ggplot2::element_text(
        margin = ggplot2::margin(b = 0.8 * base_size / 4),vjust = 0),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.8 * base_size / 4), hjust = 0.25),
      axis.text.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = 0.8 * base_size / 4), hjust = 0),

      # Legend
      legend.title = ggplot2::element_text(
        color = base_color, margin = ggplot2::margin(b = base_size * 0.4)),
      legend.title.align = 0,
      legend.text = ggplot2::element_text(
        color = base_color, size = ggplot2::rel(0.8)),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key.width = ggplot2::unit(base_size * 1.8, "pt"),
      legend.spacing = ggplot2::unit(base_size, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = ggplot2::margin(
        base_size/2, base_size/2, base_size/2, base_size/2),
      legend.position = "right",
      legend.justification = "center",
      legend.text.align = NULL,
      legend.direction = NULL,
      legend.box = NULL,
      legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
      legend.box.background = ggplot2::element_blank(),
      legend.box.spacing = ggplot2::unit(base_size, "pt"),

      # Strip
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(color = base_color),
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(b = base_size / 5, t = base_size / 5)),
      strip.text.y = ggplot2::element_text(
        angle = -90, margin = ggplot2::margin(l = base_size / 5, r = base_size / 5)),
      strip.text.y.left = ggplot2::element_text(angle = 90),
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = ggplot2::unit(base_size / 4, "pt"),
      strip.switch.pad.wrap = ggplot2::unit(base_size / 4, "pt"),

      # Panel
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(base_size / 2, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,

      # Plot
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.2), hjust = 0.5, vjust = 1,
        margin = ggplot2::margin(b = base_size)
      ),
      plot.title.position = "panel",
      plot.margin = ggplot2::margin(base_size/2, base_size/2, base_size/2, base_size/2),

      complete = TRUE
    )
}


#' @title Minimalist risk table theme for ggplot2
#' @description A minimalist \code{ggplot2} theme which removes most background elements and
#' lines.
#' @param base_size A numeric. Base font size.
#' @param base_family A numeric. Base font family.
#' @param base_color A character. Base color for lines and text.
#' @param base_line_size A numeric. Base line element size.
#' @param base_rect_size A numeric. Base rectangle element size.
#' @note Recommend exporting as PNG or TIFF to preserve
#' background transparency.
#' @seealso \code{\link{ggrisktable}}
#' @export
theme_risk <- function(
  base_size = 12,
  base_family = NULL,
  base_color = "black",
  base_line_size = base_size/12,
  base_rect_size = base_size/12
) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(

      # Defaults
      text = ggplot2::element_text(
        family = base_family,
        color = base_color,
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = ggplot2::margin(),
        debug = FALSE),
      line = ggplot2::element_line(
        color = base_color,
        linewidth = base_line_size,
        linetype = 1,
        lineend = "square"),
      rect = ggplot2::element_rect(
        fill = "transparent",
        colour = base_color,
        linewidth = base_rect_size,
        linetype = 1),

      # Axis
      axis.line = ggplot2::element_blank(),
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.title.x = NULL,
      axis.title.x.top = NULL,
      axis.title.y = NULL,
      axis.title.y.right = NULL,
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.95), color = base_color),
      axis.text.x = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0.8 * base_size / 4), hjust = 0.25),
      axis.text.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = 0.8 * base_size / 4), hjust = 0),

      # Legend
      legend.title = ggplot2::element_blank(),
      legend.title.align = 0,
      legend.text = ggplot2::element_text(
        color = base_color, size = ggplot2::rel(0.8)),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key.width = ggplot2::unit(base_size * 1.8, "pt"),
      legend.spacing = ggplot2::unit(base_size, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = ggplot2::margin(
        base_size/2, base_size/2, base_size/2, base_size/2),
      legend.position = "right",
      legend.justification = "center",
      legend.text.align = NULL,
      legend.direction = NULL,
      legend.box = NULL,
      legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
      legend.box.background = ggplot2::element_blank(),
      legend.box.spacing = ggplot2::unit(base_size, "pt"),

      # Strip
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(color = base_color),
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(b = base_size / 5, t = base_size / 5)),
      strip.text.y = ggplot2::element_text(
        angle = -90, margin = ggplot2::margin(l = base_size / 5, r = base_size / 5)),
      strip.text.y.left = ggplot2::element_text(angle = 90),
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = ggplot2::unit(base_size / 4, "pt"),
      strip.switch.pad.wrap = ggplot2::unit(base_size / 4, "pt"),

      # Panel
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(base_size / 2, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,

      # Plot
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.2), hjust = 0.5, vjust = 1,
        margin = ggplot2::margin(b = base_size)
      ),
      plot.title.position = "panel",
      plot.margin = ggplot2::margin(base_size/1.5, base_size/2, base_size/2, base_size/2),

      complete = TRUE
    )
}


#' @title Add a panel border to a ggplot2 plot
#' @description A simple \code{ggplot2} theme which replaces the axis lines with
#' a bordered panel.
#' @param base_size A numeric. Base size. Used to calculate line size and spacing.
#' @param base_color A character. Base color for lines.
#' @note This should be placed after the primary theme for the plot.
#' @examples
#' library(ggplot2)
#'
#' ggplot(datasets::mtcars, aes(x = wt, y = hp, color = as.factor(cyl))) +
#'   geom_point() +
#'   facet_wrap(~as.logical(am)) +
#'   theme_basic() +
#'   panel_border()
#' @export
panel_border <- function (base_size = 12, base_color = NULL) {
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = base_color, linewidth = base_size / 6),
    panel.spacing = ggplot2::unit(base_size / 12, 'lines')
  )
}
