#' Common contiguities of an `sf` POLYGON object
#' @description Wrapper function for [sf::st_relate()]. Computes a sparse index
#' list of contiguities for an `sf` POLYGON object.
#' @importFrom sf st_geometry_type st_relate
#' @param polygons_input object of class sf, sfc or sfg with geometry type POLYGON or MULTIPOLYGON.
#' @param contiguity character; either `"queen"`, `"rook"` or `"bishop"`. Default is `"queen"`.
#' @param fill_empty logical; should empty indices be filled? Default is `FALSE`.
#' @return A sparse index list of contiguities.
#' @details Wrapper for [sf::st_relate()] with DE9-IM predicate `F***T****` for queen
#' contiguity, `F***1****` for rook contiguity and `F***0****` for bishop contiguity.
#' Setting `fill_empty` to `TRUE` guarantees at least one index per polygon by adding self-neighboring.
#' @export
polyg_contiguity <- function(polygons_input, contiguity = "queen", fill_empty = FALSE) {
  if (!(contiguity %in% c("queen", "rook", "bishop"))) {
    stop("Contiguity should be eiter \"queen\", \"rook\" or \"bishop\"")
  }
  if (any(!unique(sf::st_geometry_type(polygons_input)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Input should be POLYGON or MULTIPOLYGON")
  }
  contiguity_pattern <- contiguity_pattern_fun(contiguity)
  contiguity_list <- sf::st_relate(polygons_input, polygons_input, pattern = contiguity_pattern)
  if (fill_empty == TRUE) {
    for (i in 1:length(contiguity_list)) {
      if (length(contiguity_list[[i]]) == 0) {
        contiguity_list[[i]] <- as.integer(i)
      }
    }
  }
  return(contiguity_list)
}
