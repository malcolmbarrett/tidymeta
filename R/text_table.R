#' Title
#'
#' @param ...
#' @param facets
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rlang !! !!!
text_table <- function(.data, study = study,  ..., group = NULL,
                       show_y_facets = TRUE, show_y_axis = TRUE,
                       show_grid = FALSE, size = 3.75) {

  .study_id <- rlang::enquo(study)
  .study_col <- rlang::quo_text(.study_id)
  .vars <- rlang::enquos(...)
  .group <- rlang::enquo(group)
  if (rlang::quo_is_null(.group)) {

    p <- dplyr::select(.data, !!.study_id, !!!.vars) %>%
      tidyr::gather(key, value, -!!.study_id) %>%
      dplyr::mutate(key = forcats::fct_inorder(key),
                    !!.study_col := lock_order(!!.study_id)) %>%
      ggplot2::ggplot(ggplot2::aes(x = 1, y = !!.study_id)) +
        ggplot2::geom_text(ggplot2::aes(label = value), size = size) +
        ggplot2::facet_grid(cols = ggplot2::vars(key), scales = "free", space = "free") +
        theme_forest() +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_blank())
  } else {
    .group_col <- rlang::quo_text(.group)
    p <- dplyr::select(.data, !!.study_id, !!.group, !!!.vars) %>%
      tidyr::gather(key, value, -!!.study_id, -!!.group)  %>%
      dplyr::mutate(key = forcats::fct_inorder(key),
                    !!.study_col := lock_order(!!.study_id),
                    !!.group_col := forcats::fct_inorder(!!.group)) %>%
      ggplot2::ggplot(ggplot2::aes(x = 1, y = !!.study_id)) +
        ggplot2::geom_text(ggplot2::aes(label = value), size = size) +
        ggplot2::facet_grid(rows = ggplot2::vars(!!.group),
                            cols = ggplot2::vars(key),
                            scales = "free", space = "free", switch = "y") +
        theme_forest() +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_blank())
  }

  if (!show_y_facets) p <- p + ggplot2::theme(strip.background.y = ggplot2::element_blank(),
                                     strip.text.y = ggplot2::element_blank())
  if (!show_grid) p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank())
  if (!show_y_axis) p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())

  p
}
