#' Compute dead ends of an sf linestring object
#'
#' @description Compute dead ends of an sf linestring object.
#'
#' @importFrom lwgeom st_startpoint st_endpoint
#' @importFrom sf st_geometry_type st_cast st_collection_extract st_intersection st_geometry st_sfc st_crs
#' @importFrom magrittr %>%
#' @param x object of class sf, sfc or sfg with geometry type LINESTRING or MULTILINESTRING.
#' @return An sf object of type POINT containing all linestring dead ends.
#' @details Given a linestring object where `A` = *intersections* and `B` = *endpoints*, the function
#' computes `A \ B`, i.e. the relative complement of `A` in `B`. If the input contains geometries of type
#' MULTILINESTRING, they are cast to LINESTRING before computing.
#' @export
st_deadends <- function(x) {
  if (!unique(sf::st_geometry_type(x)) %in% c("LINESTRING", "MULTILINESTRING")) {
    stop("Input should be LINESTRING or MULTILINESTRING")
  }
  if (unique(sf::st_geometry_type(x)) %in% c("MULTILINESTRING")) {
    x <- st_cast(x, "LINESTRING")
  }
  endpoints <- c(lwgeom::st_startpoint(x), lwgeom::st_endpoint(x))
  intersections <- sf::st_collection_extract(sf::st_intersection(x), "POINT") %>%
    sf::st_cast("POINT")
  diff <- setdiff(sf::st_geometry(endpoints), sf::st_geometry(intersections))
  deadends <- sf::st_sfc(diff, crs = sf::st_crs(x))
  return(deadends)
}
