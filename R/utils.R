#' Paste function
#'
#' Infix function to paste long strings together a little easier
#'
#' @keywords internal
#' @noRd
`%p%` <- function(lhs, rhs) {
  paste0(lhs, rhs)
}

#' @keywords internal
#' @noRd
is_tibble_installed <- function() {
  requireNamespace("tibble", quietly = TRUE)
}
