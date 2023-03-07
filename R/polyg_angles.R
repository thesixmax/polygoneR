#' Minimum and maximum angles between two `sf` LINESTRING objects
#' @description Function for computing the minimum and maximum angles 
#' between two intersecting `sf` MULTILINESTRING or LINESTRING objects.
#' @importFrom sf st_geometry_type st_geometry st_intersects st_coordinates st_sf
#' @param input1 object of class sf or sfc of type LINESTRING or MULTILINESTRING.
#' @param input2 object of class sf or sfc of type LINESTRING or MULTILINESTRING.
#' @return An sf object equivalent to `input1` with added columns containing 
#' minimum and maximum angles.
#' @details Details
#' @export
polyg_angles <- function(input1, input2) {
  if (!unique(sf::st_geometry_type(input1)) %in% c("MULTILINESTRING", "LINESTRING")) {
    stop("Input should be MULTILINESTRING or LINESTRING")
  }
  if (!unique(sf::st_geometry_type(input2)) %in% c("MULTILINESTRING", "LINESTRING")) {
    stop("Input should be MULTILINESTRING or LINESTRING")
  }
  input1_geom <- sf::st_geometry(polyg_cast_substring(input1))
  input2_geom <- sf::st_geometry(polyg_cast_substring(input2))
  intersects <- sf::st_intersects(input1_geom, input2_geom)
  angles <- mapply(function(x, y) {
    input1_coords <- data.frame(sf::st_coordinates(sf::st_geometry(x)))[, c("X", "Y")]
    input2_coords <- lapply(y, function(k) {
      input2_coords <- data.frame(sf::st_coordinates(input2_geom[k]))[, c("X", "Y")]
    })
    vals <- lapply(input2_coords, function(k) {
      a <- t(input1_coords[!interaction(input1_coords[c("X", "Y")]) %in% interaction(k[c("X", "Y")]),])
      b <- t(input1_coords[interaction(input1_coords[c("X", "Y")]) %in% interaction(k[c("X", "Y")]),])
      c <- t(k[!interaction(k[c("X", "Y")]) %in% interaction(input1_coords[c("X", "Y")]),])
      val <- angle_fun(a, b, c)
      return(val)
    })
    max_angle <- max(unlist(vals))
    min_angle <- min(unlist(vals))
    return(list(min_angle, max_angle))
  }, input1_geom, intersects)
  out <- sf::st_sf(input1_geom)
  out$min_angle <- unlist(angles[1,])
  out$max_angle <- unlist(angles[2,])
  return(out)
}



