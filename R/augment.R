#' Augment data with values from a meta-analysis model
#'
#' Augment the original data with model residuals, fitted values, and influence
#' statistics.
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
#'   augment()
#'
#' @rdname augmenters
augment.rma.uni <- function(x, ...) {
  blup0 <- purrr::possibly(metafor::blup, NULL)
  residuals0 <- purrr::possibly(stats::residuals, NULL)
  influence0 <- purrr::possibly(stats::influence, NULL)


 y <- x$yi
 pred <- blup0(x) %>% as.data.frame()
 names(pred) <- c(".fitted", ".se.fit", "conf.low.fit", "conf.high.fit")
 res <- residuals0(x)
 inf <- influence0(x)
 inf <- cbind(as.data.frame(inf$inf), dfbetas = inf$dfbs$intrcpt)
 ret <- cbind(.rownames = x$slab,
       y,
       pred,
       .resid = res,
       dplyr::select(inf, .hat = hat, .cooksd = cook.d, .std.resid = rstudent,
                     .dffits = dffits, .cov.ratio = cov.r,
                     tau.squared.del = tau2.del, qe.del = QE.del,
                     .weight = weight, .dfbetas = dfbetas)
      )
  row.names(ret) <- NULL
  if (all(ret$.rownames == seq_along(ret$.rownames))) {
    ret$.rownames <- NULL
  }
  ret
}
