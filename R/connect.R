utils::globalVariables(c('strata', 'n.risk'))

#' @title Connect tidy'd survival::survfit data to the origin of a plot
#' @description Occasionally when tidy'd survfit data is graphed in ggplot2::geom_step(),
#' the KM curve will not connect with the origin of the plot. This tool appends data
#' connecting the lines to the origin.
#' @param data Required. tibble::tibble() object. survival::survfit data that has been
#' tidy'd with broom::tidy().
#' @return A tibble containing the original data with appended points that connect the
#' curve with to the plot origin.
#' @note Adapted from the survminer package created by Alboukadel Kassambara.
#' @examples
#' library(survival)
#' library(broom) # tidy() model data
#'
#' # Data with group names specified
#' data_diabetic <- diabetic
#' data_diabetic$trt <- as.factor(data_diabetic$trt)
#' levels(data_diabetic$trt) <- c('None', 'Laser')
#'
#' # Survival Model
#' fit <- survfit(Surv(time, status) ~ trt, data = data_diabetic)
#' fit_data <- tidy(fit)
#'
#' connect_origin(fit_data)
#' @export
connect_origin <- function (data) {
  if ('n.risk' %in% colnames(data)) data <- dplyr::arrange(.data = data, dplyr::desc(n.risk))
  origin <- dplyr::distinct(.data = data, strata, .keep_all = TRUE)
  origin[intersect(c('time', 'n.censor', 'std.error', "n.event"), colnames(origin))] <- 0
  origin[c('estimate', 'conf.high', 'conf.low')] <- 1.0
  dplyr::bind_rows(origin, data)
}
