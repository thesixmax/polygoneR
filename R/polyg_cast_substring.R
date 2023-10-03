#' Cast an `sf` object to substring
#' @description Helper function for computing the substrings of an `sf` MULTILINESTRING or LINESTRING object.
#' @importFrom sf st_geometry st_geometry_type st_coordinates st_sfc st_multilinestring st_crs st_set_geometry
#' @param input_lines object of class sf or sfc with geometry type MULTILINESTRING or LINESTRING.
#' @return An sf or sfc object of type LINESTRING containing the substrings of `input_lines`.
#' @details The output is always LINESTRING of class matching the class of `input_lines`.
#' @export
polyg_cast_substring <- function(input_lines) {
  geometry <- sf::st_geometry(input_lines)
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
  lines <- sf::st_sfc(lines, crs = sf::st_crs(input_lines))
  if (class(input_lines)[1] == "sf") {
    lines <- sf::st_set_geometry(input_lines, lines)
  }
  lines <- sf::st_cast(lines, "LINESTRING", warn = FALSE)
  return(lines)
}