#' Tidying methods for meta-analyis objects
#'
#' These methods tidy the results of meta-analysis objects
#'
#' @param x a meta-analysis object. Currently supports `rma.uni` from the
#'   `metafor` package.
#' @param conf.int logical. Include confidence intervals?
#' @param exponentiate logical. Should the estimates and (if `conf.int` =
#'   `TRUE`) confidence intervals be exponentiated?
#' @param include_studies logical. Should individual studies be included in the
#'    output?
#' @param ... additional arguments
#' @param measure measure type. See [metafor::rma()]
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' library(broom)
#' library(metafor)
#' rma(yi = lnes, sei = selnes, slab = study_name, data = iud_cxca) %>%
#'   tidy()
#'
#' @rdname tidiers
tidy.rma.uni <- function(x, conf.int = TRUE, exponentiate = FALSE,
                         include_studies = TRUE, measure = "GEN", ...) {
  if (!inherits(x, "rma.uni")) stop("`x` must be of class `rma.uni`")

  estimates <- metafor::escalc(yi = x$yi.f, vi = x$vi.f, measure = measure) %>%
    metafor2df()

  estimates <- cbind(x$slab, "study", estimates[, c("yi", "sei", "zi")], NA,
                     estimates[, c("ci.lb", "ci.ub")], stringsAsFactors = FALSE)
  names(estimates) <- c("study", "type", "estimate", "std.error", "statistic",
                        "p.value", "conf.low", "conf.high")

  results <- data.frame(study = "Overall", type = "summary",
                          estimate = x$beta[1], std.error = x$se,
                          statistic = x$zval, p.value = x$pval,
                          conf.low = x$ci.lb, conf.high = x$ci.ub,
                          stringsAsFactors = FALSE)
  .data <- if (include_studies) rbind(estimates, results) else results

  if (exponentiate) {
      .data$estimate <- exp(.data$estimate)
      .data$conf.low <- exp(.data$conf.low)
      .data$conf.high <- exp(.data$conf.high)
  }

  if (!conf.int) {
    .data <- .data[-which(names(.data) %in% c("conf.low", "conf.high"))]
  }

  attributes(.data$study) <- NULL

  .data
}
