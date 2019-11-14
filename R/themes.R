#' @importFrom ggplot2 %+replace%

#' @title Minimalist white theme for ggplot2
#' @description A ggplot2 theme which removes most background
#' elements and makes all text/lines white.
#' @param base_size Optional. Numeric. Base font size. Defaults to 11.
#' @param base_family Optional. Numeric. Base font family. Defaults to ?.
#' @param base_line_size Optional. Numeric. Base line element size.
#' Defaults to base_size/22.
#' @param base_rect_size Optional. Numeric. Base rectangle element size.
#' Defaults to base_size/22.
#' @note Recommend exporting as PNG or TIFF to preserve
#' background transparency.
#' @export
theme_white <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = 'white', size = ggplot2::rel(1)),
      axis.ticks = ggplot2::element_line(color = 'white'),
      axis.title = ggplot2::element_text(color = 'white', face = 'bold'),
      axis.title.x = ggplot2::element_text(angle = 0, margin = ggplot2::unit(c(4, 0, 0, 0), "mm")),
      axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::unit(c(0, 4, 0, 0), "mm")),
      axis.text = ggplot2::element_text(color = 'white'),
      legend.title = ggplot2::element_text(color = 'white', face = 'bold'),
      legend.title.align = 0,
      legend.text = ggplot2::element_text(color = 'white'),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "transparent", colour = 'white', size = ggplot2::rel(2)),
      strip.text = ggplot2::element_text(color = 'white'),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      complete = TRUE
    )
}

#' @title Minimalist black theme for ggplot2
#' @description A ggplot2 theme which removes most background
#' elements and makes all text/lines black.
#' @param base_size Optional. Numeric. Base font size. Defaults to 11.
#' @param base_family Optional. Numeric. Base font family. Defaults to ?.
#' @param base_line_size Optional. Numeric. Base line element size.
#' Defaults to base_size/22.
#' @param base_rect_size Optional. Numeric. Base rectangle element size.
#' Defaults to base_size/22.
#' @note Recommend exporting as PNG or TIFF to preserve
#' background transparency.
#' @export
theme_black <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = 'black', size = ggplot2::rel(1)),
      axis.ticks = ggplot2::element_line(color = 'black'),
      axis.title = ggplot2::element_text(color = 'black', face = 'bold'),
      axis.title.x = ggplot2::element_text(angle = 0, margin = ggplot2::unit(c(4, 0, 0, 0), "mm")),
      axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::unit(c(0, 4, 0, 0), "mm")),
      axis.text = ggplot2::element_text(color = 'black'),
      legend.title = ggplot2::element_text(color = 'black', face = 'bold'),
      legend.title.align = 0,
      legend.text = ggplot2::element_text(color = 'black'),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "transparent", colour = 'black', size = ggplot2::rel(2)),
      strip.text = ggplot2::element_text(color = 'black'),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      complete = TRUE
    )
}

#' @title Minimalist black risk table theme for ggplot2
#' @description Ggplot2 theme which removes most background
#' elements and makes all text/lines black.
#' @param base_size Optional. Numeric. Base font size. Defaults to 10.
#' @param base_family Optional. Numeric. Base font family. Defaults to ?.
#' @param base_line_size Optional. Numeric. Base line element size.
#' Defaults to base_size/22.
#' @param base_rect_size Optional. Numeric. Base rectangle element size.
#' Defaults to base_size/22.
#' @note Recommend exporting as PNG or TIFF to preserve
#' background transparency.
#' @export
theme_risk_black <- function(
  base_size = 10,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = 'black', face = 'bold', margin = ggplot2::unit(c(0, 10, 0, 0), "mm")),
      axis.text.x = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "transparent", colour = 'black', size = ggplot2::rel(2)),
      strip.text = ggplot2::element_text(color = 'black'),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      complete = TRUE
    )
}

#' @title Minimalist white risk table theme for ggplot2
#' @description Ggplot2 theme which removes most background
#' elements and makes all text/lines white.
#' @param base_size Optional. Numeric. Base font size. Defaults to 10.
#' @param base_family Optional. Numeric. Base font family. Defaults to ?.
#' @param base_line_size Optional. Numeric. Base line element size.
#' Defaults to base_size/22.
#' @param base_rect_size Optional. Numeric. Base rectangle element size.
#' Defaults to base_size/22.
#' @note Recommend exporting as PNG or TIFF to preserve
#' background transparency.
#' @export
theme_risk_white <- function(
  base_size = 10,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = 'white', face = 'bold', margin = ggplot2::unit(c(0, 10, 0, 0), "mm")),
      axis.text.x = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "transparent", colour = 'white', size = ggplot2::rel(2)),
      strip.text = ggplot2::element_text(color = 'white'),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      complete = TRUE
    )
}
