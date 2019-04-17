#' @export
summarise_data <- function(x, ..., .fun, .args = NULL) {
  columns <- evaluate_dots(x, ...)
  dat <- data[, columns, drop = FALSE]

  aggregate(
    dat,
    FUN = .fun,
    .args
  )
}
