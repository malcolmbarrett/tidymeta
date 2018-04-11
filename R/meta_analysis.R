
#' Add a meta-analysis to a tbl
#'
#' @param x a `data.frame` produced by `tidy()`
#' @param meta a meta-analysis object (e.g. `rma.uni`)
#' @param ... additional arguments to `as.tbl`
#'
#' @return a `tbl`
#' @export
#'
#' @examples
add_meta <- function(x, meta, ...) {
  dplyr::as.tbl(x, ...) %>%
    dplyr::mutate(meta = list(meta),
      meta = ifelse(study == "Overall" & type == "summary", meta, list(NULL)))
}


#' Meta-Analysis
#'
#' A wrapper for meta-analytic functions that automatically tidies it and adds
#' the meta-analysis object to the `tbl` in a `listcol`.
#'
#' @param .data a `data.frame` containing the data for the meta-analysis
#' @param .f a meta-analysis function. Default is [metafor::rma]
#' @param ... additional arguments passed to `.f`
#' @param conf.int logical. Include confidence intervals?
#' @param exponentiate logical. Should the estimates and (if `conf.int` =
#'   `TRUE`) confidence intervals be exponentiated?
#'
#' @return a `tbl`
#'
#' @export
#'
#' @examples
#'
#' @importFrom rlang !!!
meta_analysis <- function(.data, .f = metafor::rma, ..., conf.int = TRUE,
                          exponentiate = FALSE, include_studies = TRUE,
                          bind_data = FALSE, weights = TRUE) {

  .args <- rlang::enexprs(...)

  if (dplyr::is_grouped_df(.data)) {
    group_var <- dplyr::group_vars(.data)
    .subgroup <- .data %>%
       tidyr::nest() %>%
       dplyr::mutate(meta = purrr::map(data,
              ~rlang::eval_tidy(rlang::expr(meta_analysis(.x, .f = .f, !!!.args,
                 conf.int = conf.int, exponentiate = exponentiate,
                 bind_data = bind_data, weights = FALSE)))
              )) %>%
       dplyr::select(-data) %>%
       tidyr::unnest() %>%
       dplyr::ungroup() %>%
       dplyr::mutate(study = ifelse(study == "Overall",
                                    paste("Subgroup:", !!!rlang::sym(group_var)),
                                    study))

    .meta_data <- .data %>%
       dplyr::ungroup() %>%
       meta_analysis(.f = .f, ..., conf.int = conf.int,
                   exponentiate = exponentiate, bind_data = bind_data,
                   include_studies = FALSE, weights = FALSE) %>%
       dplyr::mutate(!!group_var := "Summary") %>%
       dplyr::bind_rows(.subgroup, .)

    if (weights) .meta_data <- add_weights(.meta_data, group = !!rlang::sym(group_var))

    return(.meta_data)
  }

  .meta <- rlang::eval_tidy(rlang::expr(.f(data = .data, !!! .args)))
  .meta_data <- .meta %>%
    broom::tidy(conf.int = conf.int, exponentiate = exponentiate,
                include_studies = include_studies) %>%
    add_meta(.meta)
  if (weights) .meta_data <- add_weights(.meta_data)
  if (bind_data) {
    dplyr::bind_cols(.meta_data, dplyr::add_row(.data))
  } else {
      .meta_data
    }
}

#' Pull the meta-analysis object
#'
#' @param x a tidied meta-analysis
#'
#' @return an object created by a meta-analysis
#' @export
#'
#' @examples
pull_meta <- function(x) {
  x %>%
    dplyr::filter(study == "Overall", type == "summary") %>%
    dplyr::pull(meta) %>%
    purrr::pluck(1)
}

#' Pull the meta-analysis summary estimate
#'
#' @param x a tidied meta-analysis
#'
#' @return an object created by a meta-analysis
#' @export
#'
#' @examples
pull_summary <- function(x, conf.int = FALSE) {
  est <- x %>%
    dplyr::filter(study == "Overall", type == "summary") %>%
    dplyr::pull(estimate) %>%
    purrr::pluck(1)

  if (conf.int) {
    cil <- x %>%
      dplyr::filter(study == "Overall", type == "summary") %>%
      dplyr::pull(conf.low) %>%
      purrr::pluck(1)
    ciu <- x %>%
      dplyr::filter(study == "Overall", type == "summary") %>%
      dplyr::pull(conf.high) %>%
      purrr::pluck(1)
    est <- c(est, cil, ciu)
  }

  est
}