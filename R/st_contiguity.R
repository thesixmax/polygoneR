#' Common contiguities of an sf POLYGON object
#' @description Wrapper function for [sf::st_relate()]. Computes a sparse index list of contiguities for an sf POLYGON object.
#' @importFrom sf st_geometry_type st_relate
#' @param x object of class sf, sfc or sfg with geometry type POLYGON or MULTIPOLYGON.
#' @param contiguity character vector of length 1, which is wither "queen", "rook" or "bishop". Defaults to `"queen"`.
#' @param fill_empty logical; should empty indices be filled? Defaults to `FALSE`.
#' @return A sparse index list of contiguities.
#' @details This function is a wrapper for [sf::st_relate()] with DE9-IM predicate `F***T****` for queen
#' contiguity, `F***1****` for rook contiguity and `F***0****` for bishop contiguity.
#' Setting `fill_empty` to `TRUE` guarantees at least one index per polygon by adding self-neighboring.
#' @export
st_contiguity <- function(x, contiguity = "queen", fill_empty = FALSE) {
  if (!(contiguity %in% c("queen", "rook", "bishop"))) {
    stop("Contiguity should be eiter \"queen\", \"rook\" or \"bishop\"")
  }
  if (any(!unique(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Input should be POLYGON or MULTIPOLYGON")
  }
  if (contiguity == "queen") {
    contiguity_pattern <- "F***T****"
  }
  if (contiguity == "rook") {
    contiguity_pattern <- "F***1****"
  }
  if (contiguity == "bishop") {
    contiguity_pattern <- "F***0****"
  }
  contiguity_list <- sf::st_relate(x, x, pattern = contiguity_pattern)
  if (fill_empty == TRUE) {
    for (i in 1:length(contiguity_list)) {
      if (length(contiguity_list[[i]]) == 0) {
        contiguity_list[[i]] <- as.integer(i)
      }
    }
  }
  return(contiguity_list)
}
