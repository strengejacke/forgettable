# is string empty?
.is_empty_object <- function(x) {
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}


# dplyr >= 0.8.0 returns attribute "indices"
# grps <- attr(x, "groups", exact = TRUE)
#
# # dplyr < 0.8.0?
# if (is.null(grps)) {
#   grps <- attr(x, "indices", exact = TRUE)
#   grps <- lapply(grps, function(x) x + 1)
# } else {
#   grps <- grps[[".rows"]]
# }
#

# groups <- expand.grid(list(
#   unique(efc$c172code),
#   unique(efc$c161sex)),
#   KEEP.OUT.ATTRS = FALSE,
#   stringsAsFactors = FALSE
# )
#
# groups <- groups[order(groups$Var1, groups$Var2), ]
