#' Cast an `sf` object to substring
#' @description Helper function for computing the substrings of an `sf` MULTILINESTRING or LINESTRING object.
#' @importFrom sf st_geometry st_geometry_type st_coordinates st_sfc st_multilinestring st_crs st_set_geometry
#' @param input object of class sf or sfc with geometry type MULTILINESTRING or LINESTRING.
#' @return An sf or sfc object of type LINESTRING containing the substrings of `input`.
#' @details Function to compute the substrings of an input object. The output is always LINESTRING of class matching the `input` class.
#' @export
polyg_cast_substring <- function(input) {
  geometry <- sf::st_geometry(input)
  if (!unique(sf::st_geometry_type(geometry)) %in% c("MULTILINESTRING", "LINESTRING")) {
    stop("Input should be MULTILINESTRING or LINESTRING")
  }
  for (i in 1:length(sf::st_geometry(geometry))) {
    coords <- sf::st_coordinates(geometry[i])
    geom <- lapply(
      1:(length(coords[, 1]) - 1),
      function(k) {
        rbind(
          as.numeric(coords[k, 1:2]),
          as.numeric(coords[k + 1, 1:2])
        )
      }
    )
    geom <- sf::st_sfc(sf::st_multilinestring(geom))
    if (i == 1) { lines <- geom } else { lines <- rbind(lines, geom) }
  }
  lines <- sf::st_sfc(lines, crs = sf::st_crs(input))
  if (class(input)[1] == "sf") {
    lines <- sf::st_set_geometry(input, lines)
  }
  lines <- sf::st_cast(lines, "LINESTRING", warn = FALSE)
  return(lines)
}