#' Internal functions
#' @keywords internal
#' @noRd
contiguity_pattern_fun <- function(input) {
  if (input == "queen") {
    contiguity_pattern <- "F***T****"
  }
  if (input == "rook") {
    contiguity_pattern <- "F***1****"
  }
  if (input == "bishop") {
    contiguity_pattern <- "F***0****"
  }
  return(contiguity_pattern)
}

neighbours_fun <- function(contiguity_list) {
  list <- unlist(lapply(contiguity_list, length))
  return(list)
}
