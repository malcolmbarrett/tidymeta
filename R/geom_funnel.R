StatFunnel <- ggplot2::ggproto("StatFunnel", ggplot2::Stat,
  compute_group = function(data, scales, params, ci_level = .95,
                           summary_label = "Summary\nEstimate",
                           ci_label = NULL) {
    if (is.null(ci_label)) ci_label <- paste0(ci_level * 100, "% CI")
    ci_level <- 1 - ci_level
    se_sign <- sign(data$y)[1]
    max_se <- max(abs(data$y))
    summary_est <- data$summary[1]
    ci_lb <- summary_est - qnorm(ci_level/2, lower.tail = FALSE) * max_se
    ci_ub <- summary_est + qnorm(ci_level/2, lower.tail = FALSE) * max_se

    ci_lines <- rbind(
              data.frame(x = rep(summary_est, 2),
                         y = c(max_se, 0),
                         linetype = summary_label,
                         group = 1),
              data.frame(x = c(ci_lb, summary_est, ci_ub),
                         y = c(max_se, 0, max_se),
                         linetype = ci_label,
                         group = 2)
              )
    data$group <- NULL
    data <- cbind(ci_lines, data[1:5, -1:-2])
    data$y <- data$y * se_sign
    data
  },
  required_aes = c("x", "y", "summary"))

GeomFunnel <- ggplot2::ggproto("GeomFunnel", ggplot2::GeomLine,
                default_aes = ggplot2::aes(colour = "black", size = 0.75,
                                           linetype = 1, alpha = NA))

#' Funnel plot lines
#'
#' @inheritParams ggplot2::geom_line
#' @param ci_level the confidence level desired for the psuedo-CI lines. Between
#'   0 and 1. Also used for line labels.
#' @param summary_label a string. The legend label for the summary line.
#' @param ci_label a string. The legend label for the CI lines. Default is
#'   `NULL`, in which case the labels will be created based on `ci_level`.
#' @export
#'
#' @examples
#'
#' @importFrom purrr %||%
geom_funnel <- function(mapping = NULL, data = NULL, ci_level = .95,
                        colour = "black", color = NULL, position = "identity",
                        na.rm = FALSE, summary_label = "Summary\nEstimate",
                        ci_label = NULL, show.legend = NA, inherit.aes = TRUE,
                        ...) {

  if (is.null(mapping)) {

    mapping <- ggplot2::aes(linetype = "funnel_lines", group = 1)

  } else {
    if (is.null(mapping$linetype)) mapping$linetype <- "funnel_lines"
    if (is.null(mapping$group)) mapping$group <- 1
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatFunnel,
    geom = GeomFunnel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ci_level = ci_level,
      summary_label = summary_label,
      ci_label = ci_label,
      colour = color %||% colour,
      ...
    )
  )
}
