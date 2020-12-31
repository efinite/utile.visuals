.tabulate_at_risk <- function(fit = NULL, times = NULL) {
  fit_summary <- summary(fit, times = times)
  dplyr::bind_cols(
    strata = as.factor(
      if (is.null(fit$strata)) rep('All', length(times))
      else
        purrr::map_chr(
          as.character(fit_summary$strata),
          ~ strsplit(.x, '=')[[1]][2]
        )
    ),
    time = fit_summary$time,
    n.risk = fit_summary$n.risk
  )
}
