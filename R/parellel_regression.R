#' @title Parallel computation of multiple regression models
#' @name parallel_regression
#'
#' @description Parallel computation of multiple regression models.
#'
#' @param data A data frame, or a list of data frames.
#' @param calls Character vector, where each element is a function call to a
#'   regression. The \code{data}-argument must always have the value \code{i}.
#' @param cores Numeric, the number of cores to use.
#'
#' @return A list of regression model objects.
#'
#' @examples
#' library(lme4)
#' data(sleepstudy)
#'
#' models = c(
#'   "lme4::lmer(Reaction ~ 1 + (1 | Subject), data = i)",
#'   "lme4::lmer(Reaction ~ Days + (Days | Subject), data = i)",
#'   "lme4::lmer(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), data = i)"
#' )
#'
#' parallel_regression(sleepstudy, models)
#'
#' @export
parallel_regression <- function(data, calls) {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package `parallel` required.", call. = FALSE)
  }

  if (length(data) == 1) {
    data <- replicate(length(calls), data, simplify = FALSE)
  }

  for (i in 1:length(data)) {
    attr(data[[i]], "call") = calls[i]
  }

  names(data) <- sprintf("Model %i", 1:length(data))

  cl <- parallel::makeCluster(parallel::detectCores() - 2)
  parallel::parLapply(cl, data, function(i) eval(parse(text = attr(i, "call"))))
}
