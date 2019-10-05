utils::globalVariables(c('time', 'strata', 'n.risk'))

#' @title Create a ggplot2 table showing the number at risk
#' @description A simple wrapper function which calculates the
#' numbers at risk for a survival model and a given set of time points then
#' creates a ggplot2 table with them.
#' @param fit Required. survival::survfit() object.
#' @param times Required. Numeric. One or more time points to calculate
#' the number at risk for.
#' @param strata.order Optional. Character. Ordered names of strata factor
#' levels.
#' @return An unformatted ggplot2 table showing the number at risk.
#' @examples
#' library(survival)
#'
#' fit <- survfit(Surv(time, status) ~ trt, data = diabetic)
#'
#' ggrisktable(
#'    fit = fit,
#'    times = c(0, 10, 20, 30, 40, 50),
#'    strata.order = c('0', '1')
#' ) + theme_risk_black()
#' @export
ggrisktable <- function (fit = NULL, times = NULL, strata.order = NULL) {

  # Hard stops
  if (is.null(fit) | class(fit) != 'survfit') stop('No valid fit object provided. [Check: \'fit\']')
  if (is.null(times) | !is.numeric(times)) stop('No valid time points provided. [Check: \'times\']')
  if (!is.null(strata.order) & !is.character(strata.order)) stop('Invalid strata order data provided. [Check: \'strata.order\']')

  # Generate risk table and order
  risk_table <- utile.tools::tabulate_at_risk(fit, times)

  # Reorder strata
  if (is.character(strata.order))
    risk_table$strata <- factor(
      risk_table$strata,
      levels = unique(c(
        rev(strata.order[strata.order %in% levels(risk_table$strata)]),
        levels(risk_table$strata)
      ))
    )

  # Return plotted table
  ggplot2::ggplot(
    risk_table,
    ggplot2::aes(x = time, y = strata, label = n.risk)
  ) + ggplot2::geom_text()
}
