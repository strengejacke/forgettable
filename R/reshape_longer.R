#' @title Reshape data into long format
#' @name reshape_longer
#'
#' @description \code{reshape_longer()} reshapes one or more columns from
#'   wide into long format.
#'
#' @param x A data frame.
#' @param columns Names of variables (as character vector), or column index of
#'   variables, that should be reshaped. If multiple column groups should be
#'   reshaped, use a list of vectors (see 'Examples').
#' @param names_to todo
#' @param values_to todo
#' @param numeric_timvar todo
#' @param id todo
#'
#' @return A reshaped data frame.
#'
#' @examples
#' d <- data.frame(
#'   x = 1:4,
#'   y1 = rnorm(4),
#'   y2 = rnorm(4),
#'   a = c(1, 1, 0, 0),
#'   b = c(0, 1, 1, 1)
#' )
#'
#' # simple reshape of one time-varying variable
#' reshape_longer(
#'   d,
#'   columns = c("y1", "y2"),
#'   names_to = "time",
#'   values_to = "new_y"
#' )
#'
#' d <- data.frame(
#'   x = 1:4,
#'   y1 = rnorm(4),
#'   y2 = rnorm(4),
#'   z1 = rep(3, 4),
#'   z2 = rep(-2, 4),
#'   a = c(1, 1, 0, 0),
#'   b = c(0, 1, 1, 1)
#' )
#'
#' # reshape multiple time-varying variables
#' reshape_longer(
#'   d,
#'   columns = list(c("y1", "y2"), c("z1", "z2")),
#'   names_to = "time",
#'   values_to = c("new_y", "new_z")
#' )
#'
#' @importFrom stats reshape
#' @export
reshape_longer <- function(x, columns = colnames(x), names_to = "key", values_to = "value", numeric_timvar = FALSE, id = "id") {

  ## TODO make timevar numeric?

  variable_attr <- lapply(x, attributes)

  if (is.numeric(columns)) columns <- colnames(x)[columns]
  if (!is.list(columns)) columns <- list(columns)

  dat <- stats::reshape(
    x,
    idvar = id,
    times = columns[[1]],
    timevar = names_to,
    v.names = values_to,
    varying = columns,
    direction = "long"
  )

  if (numeric_timvar) {
    f <- as.factor(dat[[names_to]])
    levels(f) <- 1:nlevels(f)
    dat[[names_to]] <- as.numeric(as.character(f))
  }

  for (i in colnames(dat)) {
    attributes(dat[[i]]) <- variable_attr[[i]]
  }

  dat
}