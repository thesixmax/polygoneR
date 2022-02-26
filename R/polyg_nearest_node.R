#' Shortest linestring connections from an `sf` POINT object to nodes of a `sf` LINESTRING object
#' @description Compute the minimum distance linestring connection from an `sf` POINT object to nodes of an `sf` LINESTRING object
#' @importFrom sf st_nearest_points st_intersects st_length st_sf st_crs st_make_valid st_collection_extract
#' st_set_crs st_crs
#' @importFrom future.apply future_lapply
#' @param points_input object of class sf, sfc or sfg with geometry type POINT or MULTIPOINT.
#' @param lines_input object of class sf, sfc or sfg with geometry type LINESTRING or MULTILINESTRING.
#' @param parallel logical; should the computation be in parallel? Default is `FALSE`. 
#' Computation in parallel requires installation of `future.apply` and setting a `plan()`.
#' @return An `sf` object of type LINESTRING containing the minimum distance linestring
#' connections from each point in `points_input` to nodes in `lines_input`.
#' @details For each point, the function computes the minimum distance to the nodes of a set of
#' linestrings and outputs the minimum distance linestring. If the point already intersects
#' with an existing node, that particular node is disregarded in the computation.
#' This guarantees linestrings to have length > 0. If `points_input` or `lines_input` contain
#' geometries of type MULTIPOINT or MULTILINESTRING, they are converted to POINT and LINESTRING before computing.
#' @export
polyg_nearest_node <- function(points_input, lines_input, parallel = FALSE) {
  if (any(!unique(sf::st_geometry_type(lines_input)) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Lines input should be LINESTRING or MULTILINESTRING")
  }
  if (any(unique(sf::st_geometry_type(lines_input)) %in% c("MULTILINESTRING"))) {
    lines_input <- sf::st_cast(sf::st_cast(lines_input, "MULTILINESTRING", warn = FALSE), "LINESTRING", warn = FALSE)
  }
  if (any(!unique(sf::st_geometry_type(points_input)) %in% c("POINT", "MULTIPOINT"))) {
    stop("Point input should be POINT or MULTIPOINT")
  }
  if (any(unique(sf::st_geometry_type(points_input)) %in% c("MULTIPOINT"))) {
    points_input <- sf::st_cast(sf::st_cast(points_input, "MULTIPOINT", warn = FALSE), "POINT", warn = FALSE)
  }
  if (any(!(class(points_input)) %in% c("sfc_POINT", "sfc"))) {
    points_input <- sf::st_sfc(sf::st_geometry(points_input))
  }
  if (sf::st_crs(points_input) != sf::st_crs(lines_input)) {
    stop("CRS of lines and points input are not the same")
  }
  linestrings_fun <- function(x) {
    no_intersection <- is.na(as.numeric(unlist(sf::st_intersects(points_input[x], lines_input))[1]))
    nearest_points <- sf::st_nearest_points(
      points_input[x],
      if(no_intersection == TRUE) {
        lines_input
      } else {
        lines_input[(-c(as.numeric(unlist(sf::st_intersects(points_input[x],lines_input))))),]
      }
    )
    distances <- sf::st_length(nearest_points)
    lines_connection <- nearest_points[which.min(distances)]
    return(lines_connection)
  }
  start_time <- Sys.time()
  message("Computing nearest nodes")
  lines_process <-
    if (parallel == TRUE) {
      future.apply::future_lapply(points_input, linestrings_fun, future.seed = TRUE)
    } else {
      lapply(points_input, linestrings_fun)
    }
  lines_output <- sf::st_as_sf(sf::st_sfc(unlist(lines_process, recursive = FALSE)))
  lines_output <- rename_geometry(lines_output, "geometry")
  lines_output <- sf::st_set_crs(lines_output, sf::st_crs(lines_input))
  end_time <- Sys.time()
  message(paste("Nearest nodes successfully computed in", round(difftime(end_time, start_time, units = "mins"), 2), "minutes"))
  return(lines_output)
}
