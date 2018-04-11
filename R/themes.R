#' Minimalist theme with forest plots in mind
#'
#' @param font_size
#' @param font_family
#' @param line_size
#'
#' @return
#' @export
#'
#' @examples
theme_forest <- function(font_size = 14, font_family = "", line_size = .5) {
  list(
  ggplot2::theme_minimal() %+replace%
  ggplot2::theme(
        text = ggplot2::element_text(family = font_family, face = "plain", colour = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = ggplot2::margin(), debug = FALSE),
        strip.text.y = ggplot2::element_text(angle = 180),
        strip.background = ggplot2::element_rect(color = "grey86", fill = "grey92"),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 12, hjust = 1),
        axis.text.x = ggplot2::element_text(size = 12),
        legend.position = "none",
        complete = TRUE
        ),
  ggplot2::scale_shape_manual(values = c(15, 18)),
  ggplot2::scale_size_continuous(range = c(3, 10))
  )
}
