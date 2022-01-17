#' Compute a linestring connection from an sf POINT object to nodes of a sf LINESTRING object
#' @description Compute the minimum distance linestring connection from an sf POINT object to nodes of an sf LINESTRING object
#' @importFrom sf st_nearest_points st_intersects st_length st_sf st_crs st_make_valid
#' @importFrom dplyr slice rename
#' @importFrom magrittr %>%
#' @importFrom future.apply future_lapply
#' @param points object of class sf, sfc or sfg with geometry type POINT or MULTIPOINT.
#' @param lines object of class sf, sfc or sfg with geometry type LINESTRING or MULTILINESTRING.
#' @param parallel logical; should the computation be in parallel? Defaults to FALSE.
#' @return An sf object of type LINESTRING containing the minimum distance linestring
#' connection from the `point` input to the nodes of the `lines` input.
#' @details Given a point, the function computes the minimum distance tocthe nodes of a set of
#' linestrings and outputs the relevant linestring. If the point overlaps
#' with a node, this node is not considered in the computation.
#' This guarantees linestrings to have lengh > 0.
#' @export
st_nearest_node <- function(points, lines, parallel = FALSE) {
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
    sf::st_make_valid()
  return(lines_tmp)
}
