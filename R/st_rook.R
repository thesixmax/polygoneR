#' Compute rook contiguity of an sf object
#' @description Wrapper function for [sf::st_relate()]. Computes a sparse index list of rook contiguities for an sf POLYGON object.
#' @importFrom sf st_geometry_type st_relate
#' @param x object of class sf, sfc or sfg with geometry type POLYGON or MULTIPOLYGON.
#' @param fill_empty logical; should empty indices be filled? Defaults to `FALSE`.
#' @return A sparse index list of rook contiguities.
#' @details This function is a wrapper for [sf::st_relate()] with DE9-IM predicate `F***1****`.
#' Setting `fill_empty` to `TRUE` guarantees at least one index per polygon by adding self-neighboring.
#' @export
st_rook <- function(x, fill_empty = FALSE) {
  if (!unique(sf::st_geometry_type(x)) %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("Input should be POLYGON or MULTIPOLYGON")
  }
  rook_list <- sf::st_relate(x, x, pattern = "F***1****")
  if(fill_empty == TRUE) {
    for (i in 1:length(rook_list)) {
      if(length(rook_list[[i]]) == 0) {
        rook_list[[i]] <- as.integer(i)
      }
    }
  }
  return(rook_list)
}
