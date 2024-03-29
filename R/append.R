#' @title Append a ggplot2 table to the bottom of a ggplot2 plot
#' @description Aligns axes and combines a ggplot2 plot and table into a single plot.
#' Can handle legends.
#' @param plot A ggplot2::ggplot() object. If a legend is present, it will be
#' extracted.
#' @param table A ggplot2::ggplot object. If a legend is present, it will
#' be removed and ignored.
#' @param plot.height A numeric. Height of plot relative to table. Defaults
#' to 1.
#' @param table.height A numeric. Height of table relative to plot. Defaults
#' to 0.1.
#' @param plot.width A numeric. Width of plot relative to legend. Ignored
#' if no legend present in plot. Defaults to 1.
#' @param legend.width A numeric. Width of legend relative to plot. Ignored
#' if no legend present in plot. Defaults 0.2.
#' @param legend.offset A numeric. Vertical offset of legend box. Used to raise
#' or lower. Ignored if no legend present in plot. Defaults
#' to -15.
#' @return A ggplot2 tableGrob object. Use grid::grid.draw() to open in RStudio viewer.
#' Works with ggplot2::ggsave() out of the box.
#' @note To ensure proper alignment, double check that both plots use the same scale
#' and breaks!
#' @examples
#' library(survival)
#' library(ggplot2)
#' library(grid) # grid.draw() finished plot
#'
#' # Data with group names specified
#' data_diabetic <- diabetic
#' data_diabetic$trt <- as.factor(data_diabetic$trt)
#' levels(data_diabetic$trt) <- c('None', 'Laser')
#'
#' # Survival Model
#' fit <- survfit(Surv(time, status) ~ trt, data = data_diabetic)
#' fit <- survfit0(fit)
#'
#' # Kaplan Meier (KM) Plot
#' plot_km <- ggplot(
#'  data = data.frame(
#'   time = fit$time,
#'   surv = fit$surv,
#'   conf.low = fit$lower,
#'   conf.high = fit$upper,
#'   strata = rep(names(fit$strata), fit$strata)
#'  ),
#'  mapping = aes(x = time, y = surv)
#' ) +
#'   geom_step(aes(color = strata)) +
#'   geom_stepconfint(aes(ymin = conf.low, ymax = conf.high, fill = strata), alpha = 0.3) +
#'   coord_cartesian(c(0, 50)) + # Note scale set here!
#'   scale_x_continuous(expand = c(0.02,0)) +
#'   labs(x = 'Time', y = 'Freedom From Event') +
#'   scale_color_manual(
#'     values = c('#d83641', '#1A45A7'),
#'     name = 'Treatment',
#'     labels = c('Laser', 'None'),
#'     aesthetics = c('colour', 'fill')) +
#'   theme_basic()
#'
#' # Risk Table
#' tbl_risk <- ggrisktable(fit, c(0, 10, 20, 30, 40, 50)) +
#'   coord_cartesian(c(0, 50)) +
#'   scale_x_continuous(expand = c(0.02,0)) +
#'   theme_risk()
#'
#' # Combine KM plot and risk table
#' plot_cmbd <- append_table(
#'   plot = plot_km,
#'   table = tbl_risk
#' )
#'
#' # Draw in RStudio viewer
#' grid.newpage()
#' grid.draw(plot_cmbd)
#' @export
append_table <- function(
  plot = NULL, table = NULL, plot.height = 1,
  table.height = 0.1, plot.width = 1, legend.width = 0.2, legend.offset = -15
) {

  # Hard stops
  if (!ggplot2::is.ggplot(plot) | !ggplot2::is.ggplot(table))
    stop('Missing valid ggplot object. Check [\'plot\', \'table\'].')
  if (!is.numeric(plot.height) | !is.numeric(table.height))
    stop('Both plot and table heights must be specified. Check \'plot.height\', \'table.height\'.')

  # Convert plots to grobs
  plot <- plot +
    ggplot2::theme(
      legend.box.margin = ggplot2::margin(legend.offset, 0, 0, 0, unit = 'mm'),
      legend.justification = NULL,
      legend.position = NULL
    )
  grob_plot <- ggplot2::ggplotGrob(plot)
  grob_tbl <- ggplot2::ggplotGrob(table + ggplot2::theme(legend.position = 'none')) # drop table legend

  # Extract legend, if present
  legend_row <- which(purrr::map_chr(grob_plot$grobs, ~ .x$name) == 'guide-box')
  if (length(legend_row) > 0) {

    # Hard stop
    if (!is.numeric(plot.width) | !is.numeric(legend.width))
      stop('Widths for both plot & legend required. Check [\'legend.width\', \'plot.width\'].')

    grob_legend <- grob_plot$grobs[[legend_row]]
    grob_plot <- ggplot2::ggplotGrob(plot + ggplot2::theme(legend.position = 'none'))
  }

  # Combine plot grobs
  grob_combined <- rbind(
    grob_plot,
    grob_tbl,
    size = 'first'
  )
  panels <- grob_combined$layout$t[grep("panel", grob_combined$layout$name)]
  grob_combined$heights[panels[1]] <- ggplot2::unit(plot.height, "null") # Set plot height
  grob_combined$heights[panels[2]] <- ggplot2::unit(table.height, "null") # Set table height

  # Return new arrangement
  if (exists('grob_legend'))
    gridExtra::arrangeGrob(
      grob_combined,
      grob_legend,
      ncol = 2,
      widths = c(plot.width, legend.width),
      clip = TRUE
    )
  else grob_combined
}
