#' Calculate the minimum distance linestring from an `sf` POINT object to nodes of a `sf` LINESTRING object
#' @description Calculate the minimum distance linestring connection from an `sf` POINT object to nodes of an `sf` LINESTRING object
#' @importFrom sf st_geometry_type st_intersection st_geometry st_cast st_collection_extract 
#' st_nearest_feature st_sfc st_union st_crs st_equals
#' @param input_points object of class sf or sfc with geometry type POINT or MULTIPOINT.
#' @param input_lines object of class sf or sfc with geometry type LINESTRING or MULTILINESTRING.
#' @param cast_substring logical; should the input linestring be cast to substring? Default is `TRUE`.
#' @param ignore_equal logical; should output linestrings which are exactly equal to 
#' one or more elements in `input_lines` be ignored? Default is `TRUE`.
#' @return An `sf` object of type LINESTRING containing the minimum distance linestring
#' connections from each point in `input_points` to nodes in `input_lines`.
#' @details For each point, the function computes the minimum distance to the intersection nodes of a set of
#' linestrings and outputs the minimum distance linestrings. Empty linestrings as a result of 
#' equality between `input_points` and nodes in `input_lines`. If `input_points` or `input_lines` contain
#' geometries of type MULTIPOINT or MULTILINESTRING, they are converted to POINT and LINESTRING respectively before computing.
#' @export
polyg_line_nearest_node <- function(input_points, input_lines, cast_substring = TRUE, ignore_equal = TRUE) {
  if (any(!unique(sf::st_geometry_type(input_lines)) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Lines input should be LINESTRING or MULTILINESTRING")
  }
  if (any(!unique(sf::st_geometry_type(input_points)) %in% c("POINT", "MULTIPOINT"))) {
    stop("Point input should be POINT or MULTIPOINT")
  }
  if (any(unique(sf::st_geometry_type(input_lines)) %in% c("MULTILINESTRING"))) {
    input_lines <- sf::st_cast(sf::st_cast(input_lines, "MULTILINESTRING", warn = FALSE), "LINESTRING", warn = FALSE)
  }
  if (any(unique(sf::st_geometry_type(input_points)) %in% c("MULTIPOINT"))) {
    input_points <- sf::st_cast(sf::st_cast(input_points, "MULTIPOINT", warn = FALSE), "POINT", warn = FALSE)
  }
  if(cast_substring) {
    l_select <- sf::st_geometry(polyg_cast_substring(input_lines))
  } else {
    l_select <- sf::st_geometry(input_lines)
  }
  l_inter <- sf::st_intersection(sf::st_geometry(l_select), sf::st_geometry(l_select))
  if(any(unique(sf::st_geometry_type(l_inter)) %in% c("POINT", "MULTIPOINT"))) {
    l_nodes <- sf::st_cast(sf::st_union(sf::st_collection_extract(l_inter, "POINT")), "POINT", warn = FALSE)
  } else {
    stop("No valid nodes in line input")
  }
  nn_id <- sf::st_nearest_feature(input_points, l_nodes)
  nn_filter <- sf::st_geometry(l_nodes)[nn_id]
  lnn <- sf::st_sfc(mapply(function(a, b) {
    union <- sf::st_union(a, b)
    if(sf::st_geometry_type(union) %in% "MULTIPOINT") {
      lnn <- sf::st_cast(union, "LINESTRING", warn = FALSE)
    } else {
      lnn <- NULL
    }
    return(lnn)
  }, sf::st_geometry(input_points), sf::st_geometry(nn_filter), SIMPLIFY = FALSE))
  sf::st_crs(lnn) <- sf::st_crs(input_lines)
  if(ignore_equal) {
    lnn_equal <- lengths(sf::st_equals(lnn, l_select))
    lnn <- lnn[which(lnn_equal == 0)]
  }
  lnn <- lnn[which(!sf::st_is_empty(lnn))]
  return(lnn)
}
