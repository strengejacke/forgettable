evaluate_dots <- function(x, ...) {
  dots <- try(list(...), silent = TRUE)

  if (.is_empty_object(dots)) {
    columns <- colnames(x)
  } else if (inherits(dots, "try-error")) {
    vars <- deparse(substitute(list(...)))
    vars <- gsub("list\\((.*)\\)", "\\1", vars)

    if (any(grepl(",", vars, fixed = TRUE))) {
      columns <- unlist(strsplit(vars, ","))
    } else if (any(grepl(":", vars, fixed = TRUE))) {
      start_end <- unlist(strsplit(vars, ":", fixed = TRUE))
      start <- which(colnames(x) == start_end[1])
      end <- which(colnames(x) == start_end[2])
      columns <- colnames(x)[start:end]
    } else {
      columns <- vars
    }

  } else {
    dots <- lapply(dots, function(vars) {
      if (any(grepl(",", vars, fixed = TRUE))) {
        unlist(strsplit(vars, ","))
      } else if (any(grepl(":", vars, fixed = TRUE))) {
        start_end <- unlist(strsplit(vars, ":", fixed = TRUE))
        start <- which(colnames(x) == start_end[1])
        end <- which(colnames(x) == start_end[2])
        colnames(x)[start:end]
      } else {
        vars
      }

    })
    columns <- unlist(dots)
  }

  columns
}
