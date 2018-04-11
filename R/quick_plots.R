#' Title
#'
#' @param x
#' @param size
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @rdname quickplots
#'
#' @importFrom rlang !!
forest_plot <- function(x, estimate = estimate, study = study, size = weight,
                         shape = type, col = type, xmin = conf.low,
                         xmax = conf.high, group = NULL, alpha = .75, height = 0,
                         ...) {

  .estimate <- rlang::enquo(estimate)
  .study <- rlang::enquo(study)
  .studycol <- rlang::quo_text(.study)
  .size <- rlang::enquo(size)
  .shape <- rlang::enquo(shape)
  .col <- rlang::enquo(col)
  .group <- rlang::enquo(group)
  .groupcol <- rlang::quo_text(.group)
  .xmin <- rlang::enquo(xmin)
  .xmax <- rlang::enquo(xmax)

 if (is.null(x$weight)) {

 }

 x <- x %>%
     dplyr::mutate(!!.studycol := lock_order(!!.study))

 if (!rlang::quo_is_null(.group)) {
   x <- x %>%
       dplyr::mutate(!!.groupcol := lock_order(!!.group, rev = FALSE))
 }

  p <- x %>%
    ggplot2::ggplot(ggplot2::aes(x = !!.estimate, y = !!.study,
                                 shape = !!.shape, col = !!.col)) +
      ggplot2::geom_point(ggplot2::aes(size = !!.size), alpha = alpha) +
      theme_forest()

  if (!rlang::quo_is_null(.xmin)) {
      p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin = !!.xmin,
                                                    xmax = !!.xmax),
                                                    height = height,
                                                    alpha = alpha)
    }

  if (!rlang::quo_is_null(.group)) {
    p <- p + ggplot2::facet_grid(ggplot2::vars(!!.group), scales = "free",
                                 space = "free", switch = "y")
    }
  p
}

#' @rdname quickplots
#' @export
#' @importFrom rlang !!
influence_plot <- function(x, estimate = l1o_estimate, study = study, size = 4,
                         shape = 15, col = type, xmin = l1o_conf.low,
                         xmax = l1o_conf.high, group = NULL, alpha = .75, height = 0,
                         ...) {

  .estimate <- rlang::enquo(estimate)
  .study <- rlang::enquo(study)
  .col <- rlang::enquo(col)
  .studycol <- rlang::quo_text(.study)
  .group <- rlang::enquo(group)
  .groupcol <- rlang::quo_text(.group)
  .xmin <- rlang::enquo(xmin)
  .xmax <- rlang::enquo(xmax)


 vline_data <- data.frame(cuts = pull_summary(x, conf.int = TRUE),
                          type = c("Estimate", "95% CI", "95% CI"))
 x <- x %>%
     dplyr::filter(!stringr::str_detect(study, "Subgroup")) %>%
     dplyr::mutate(!!.studycol := lock_order(!!.study))

 if (!rlang::quo_is_null(.group)) {
   x <- x %>%
       dplyr::mutate(!!.groupcol := lock_order(!!.group, rev = FALSE))
 }



  p <- x %>%
    dplyr::mutate(estimate = !!.estimate, !!.studycol := lock_order(!!.study)) %>%
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = !!.study, col = forcats::fct_rev(!!.col))) +
      ggplot2::geom_point(shape = shape, size = size, alpha = alpha) +
      ggplot2::geom_vline(data = vline_data, ggplot2::aes(xintercept = cuts, linetype = type)) +
      theme_forest()

  if (!rlang::quo_is_null(.xmin)) {
      p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin = !!.xmin,
                                                    xmax = !!.xmax),
                                                    height = height,
                                                    alpha = alpha)
    }

  if (!rlang::quo_is_null(.group)) {
    p <- p + ggplot2::facet_grid(ggplot2::vars(!!.group), scales = "free",
                                 space = "free", switch = "y")
    }
  p
}


#' Title
#'
#' @param study
#'
#' @return
#' @export
#'
#' @examples
lock_order  <- function(study, rev = TRUE) {
  study <- forcats::fct_inorder(study)
  if (rev) study <- forcats::fct_rev(study)
  study
}

#' Title
#'
#' @param x
#' @param group
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom rlang !!
sub2summary <- function(x, group, ...) {
  .group <- rlang::enquo(group)
  .groupcol <- rlang::quo_text(.group)
  x %>%
    dplyr::mutate(!!.groupcol := ifelse(stringr::str_detect(study, "Subgroup"),
                                        "Summary",
                                        !!.group)) %>%
    dplyr::arrange(type)
}

