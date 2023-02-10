#' Rename the geometry of an `sf` object
#' @description Simple renaming function for the geometry column of sf objects
#' @importFrom sf st_geometry
#' @param input object of class sf
#' @param geom_name desired name for the geometry column. Defaults to "geometry"
#' @return The input sf object with renamed geometry column
#' @export
polyg_rename_geom <- function(input, geom_name = "geometry") {
  if(class(input)[1] != "sf") {
    stop("Input has to be of class sf")
  }
  current <- attr(input, "sf_column")
  names(input)[names(input) == current] <- geom_name
  sf::st_geometry(input) <- geom_name
  return(input)
}
