#' Minimalist theme with meta-analyses in mind
#'
#' `theme_meta()` is a base theme, while `theme_forest()` has forest plots in
#' mind, meaning it removes most legends and sets squares and diamonds as the
#' default point shapes.
#'
#' @param font_size font size
#' @param font_family font family
#' @param line_size line size
#'
#' @export
#'
#' @rdname metathemes
#' @importFrom ggplot2 %+replace%
theme_meta <- function(font_size = 14, font_family = "", line_size = .5) {
  ggplot2::theme_minimal() %+replace%
  ggplot2::theme(
        text = ggplot2::element_text(family = font_family, face = "plain", colour = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = ggplot2::margin(), debug = FALSE),
        strip.text.y = ggplot2::element_text(angle = 180),
        strip.background = ggplot2::element_rect(color = "grey86", fill = "grey92"),
        axis.text.y = ggplot2::element_text(size = 12, hjust = 1),
        axis.text.x = ggplot2::element_text(size = 12),
        complete = TRUE
        )
}

#' @export
#' @rdname metathemes
theme_forest <- function(font_size = 14, font_family = "", line_size = .5) {
  list(
  theme_meta(font_size = font_size, font_family = font_family, line_size = line_size),
  ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                 legend.position = "none"),
  ggplot2::scale_shape_manual(values = c(15, 18)),
  ggplot2::scale_size_continuous(range = c(3, 10))
  )
}
