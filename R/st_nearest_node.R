#' Shortest linestring connections from an sf POINT object to nodes of a sf LINESTRING object
#' @description Compute the minimum distance linestring connection from an sf POINT object to nodes of an sf LINESTRING object
#' @importFrom sf st_nearest_points st_intersects st_length st_sf st_crs st_make_valid st_collection_extract
#' @importFrom dplyr slice rename filter
#' @importFrom magrittr %>%
#' @importFrom future.apply future_lapply
#' @param points object of class sf, sfc or sfg with geometry type POINT or MULTIPOINT.
#' @param lines object of class sf, sfc or sfg with geometry type LINESTRING or MULTILINESTRING.
#' @param parallel logical; should the computation be in parallel? Defaults to FALSE.
#' @return An sf object of type LINESTRING containing the minimum distance linestring
#' connection from the `points` input to nodes of the `lines` input.
#' @details Given a point, the function computes the minimum distance to the nodes of a set of
#' linestrings and outputs the relevant linestring. If the point already overlaps
#' with an existing node, this node is disregarded.
#' This guarantees linestrings to have length > 0. If the `points` or `lines` input contains
#' geometries of type MULTIPOINT or MULTILINESTRING, they are converted to POINT and LINESTRING before computing.
#' @export
st_nearest_node <- function(points, lines, parallel = FALSE) {
  if (any(!unique(sf::st_geometry_type(lines)) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Lines input should be LINESTRING or MULTILINESTRING")
  }
  if (any(unique(sf::st_geometry_type(lines)) %in% c("MULTILINESTRING"))) {
    input <- sf::st_cast(sf::st_cast(lines, "MULTILINESTRING", warn = FALSE), "LINESTRING", warn = FALSE)
  }
  if (any(!unique(sf::st_geometry_type(points)) %in% c("POINT", "MULTIPOINT"))) {
    stop("Point input should be POINT or MULTIPOINT")
  }
  if (any(unique(sf::st_geometry_type(points)) %in% c("MULTIPOINT"))) {
    input <- sf::st_cast(sf::st_cast(points, "MULTIPOINT", warn = FALSE), "POINT", warn = FALSE)
  }
  linestrings_fun <- function(x) {
    nearest_points <- sf::st_nearest_points(
      points[x],
      lines %>%
        dplyr::slice(-c(as.numeric(unlist(
          sf::st_intersects(
            points[x],
            lines
          )
        ))))
    )
    distances <- sf::st_length(nearest_points)
    lines_connection <- nearest_points[which.min(distances)]
    return(lines_connection)
  }
  lines_process <-
    if (parallel == TRUE) {
      future_lapply(points, linestrings_fun, future.seed = TRUE)
    } else {
      lapply(points, linestrings_fun)
    }
  lines_tmp <- unlist(lines_process, recursive = FALSE)
  lines_output <- sf::st_sf(lines_tmp, crs = sf::st_crs(lines), geom = NA) %>%
    dplyr::rename("geometry" = "lines_tmp") %>%
    dplyr::select(-"geom") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("LINESTRING")
  return(lines_output)
}
