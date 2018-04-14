#' Construct a one-row summary of meta-analysis model fit statistics.
#'
#' `glance()` computes a one-row summary of  meta-analysis objects,
#'  including estimates of heterogenity and model fit.
#'
#' @param x a meta-analysis object. Currently supports `rma.uni` from the
#'   `metafor` package.
#' @param ... additional arguments
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#'
#' library(broom)
#' library(metafor)
#' rma(yi = lnes, sei = selnes, slab = study_name, data = iud_cxca) %>%
#'   glance()
#'
glance.rma.uni <- function(x, ...) {
  fit_stats <- metafor::fitstats(x)
  fit_stats <- fit_stats %>%
    t() %>%
    as.data.frame()
  names(fit_stats) <- stringr::str_replace(names(fit_stats), "\\:", "")
  data.frame(i.squared = x$I2, h.squared = x$H2, tau.squared = x$tau2, fit_stats)
}
