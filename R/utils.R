metafor2df <- function(x) {
  x %>%
    summary() %>%
    as.data.frame(stringsAsFactors = FALSE)
}
