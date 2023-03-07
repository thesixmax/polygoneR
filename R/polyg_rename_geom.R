#' Rename the geometry column of an `sf` object
#' @description Simple renaming function for the geometry column of sf objects
#' @importFrom sf st_geometry
#' @param input_sf object of class sf
#' @param geom_name desired name for the geometry column. Defaults to "geometry"
#' @return The input sf object with renamed geometry column
#' @export
polyg_rename_geom <- function(input_sf, geom_name = "geometry") {
  if(class(input_sf)[1] != "sf") {
    stop("Input has to be of class sf")
  }
  current <- attr(input_sf, "sf_column")
  names(input_sf)[names(input_sf) == current] <- geom_name
  sf::st_geometry(input_sf) <- geom_name
  return(input_sf)
}
