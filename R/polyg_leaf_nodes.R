#' Identify leaf nodes of an `sf` LINESTRING object
#' @description Helper function for computing leaf nodes of an `sf` MULTILINESTRING or LINESTRING object.
#' @importFrom sf st_geometry_type st_cast st_sfc st_geometry st_union st_intersects st_sf
#' @param input_lines object of class sf of type LINESTRING or MULTILINESTRING.
#' @return An sf object of type POINT containing all leaf nodes of `input`.
#' @details Function to compute leaf nodes, i.e. nodes which only intersect
#' with one linestring in the `sf` LINESTRING object. If `input` contains geometries
#' of type MULTILINESTRING, they are cast to LINESTRING before computing.
#' @export
polyg_leaf_nodes <- function(input_lines) {
  if (!unique(sf::st_geometry_type(input_lines)) %in% c("MULTILINESTRING", "LINESTRING")) {
    stop("Input should be MULTILINESTRING or LINESTRING")
  }
  if (any(unique(sf::st_geometry_type(input_lines)) %in% c("MULTILINESTRING"))) {
    input_lines <- sf::st_cast(sf::st_cast(input_lines, "MULTILINESTRING", warn = FALSE), "LINESTRING", warn = FALSE)
  }
  ls <- polyg_cast_substring(sf::st_sfc(sf::st_geometry(input_lines)))
  nodes <- sf::st_union(sf::st_cast(ls, "POINT"), by_feature = TRUE)
  n_inter <- lengths(sf::st_intersects(nodes, ls))
  leaf_nodes <- polyg_rename_geom(sf::st_sf(nodes[which(n_inter == 1)]), "geometry")
  return(leaf_nodes)
}
