#' @title Reshape data into long format
#' @name reshape_longer
#'
#' @description \code{reshape_longer()} reshapes one or more columns from
#'   wide into long format.
#'
#' @param x A data frame.
#' @param names_to todo
#' @param values_to todo
#' @param columns todo
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
#'   d, names_to = "time", values_to = "new_y", columns = c("y1", "y2")
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
#'   d, names_to = "time", values_to = c("new_y", "new_z"),
#'   columns = list(c("y1", "y2"), c("z1", "z2"))
#' )
#'
#' @importFrom stats reshape
#' @export
reshape_longer <- function(x, names_to = "key", values_to = "value", columns = colnames(x)) {

  ## TODO preserve attributes
  ## TODO keep id-var?
  ## TODO make timevar numeric?

  if (is.numeric(columns)) columns <- colnames(x)[columns]
  if (!is.list(columns)) columns <- list(columns)

  stats::reshape(
    x,
    idvar = "id",
    times = columns[[1]],
    timevar = names_to,
    v.names = values_to,
    varying = columns,
    direction = "long"
  )
}