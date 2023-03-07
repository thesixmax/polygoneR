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
angle_fun <- function(a, b, c) {
  vector1 <- c(a[1] - b[1], a[2] - b[2])
  vector2 <- c(c[1] - b[1], c[2] - b[2])
  num <- vector1[1] * vector2[1] + vector1[2] * vector2[2]
  den <- sqrt(vector1[1]^2 + vector1[2]^2) * sqrt(vector2[1]^2 + vector2[2]^2)
  angle <- acos(num / den)
  angle <- (360 * angle) / (2 * pi)
  return(angle)
}
