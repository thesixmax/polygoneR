#' Leaf nodes of an `sf` LINESTRING object
#' @description Helper function for computing leaf nodes of an `sf` LINESTRING object.
#' @importFrom lwgeom st_startpoint st_endpoint
#' @importFrom sf st_geometry_type st_cast st_collection_extract st_intersection
#' st_geometry st_sfc st_crs
#' @importFrom future.apply future_lapply
#' @param lines_input object of class sf, sfc or sfg with geometry type
#' LINESTRING or MULTILINESTRING.
#' @param parallel logical; should the computation be in parallel? Default is `FALSE`.
#' @param n_cores integer; number of cores if run in parallel. Should match number of workers set with [future::plan()].
#' @param verbose logical; should computing time be printed?
#' @return An sfc object of type POINT containing all leaf nodes of `lines_input`.
#' @details Function to compute leaf nodes, i.e. nodes which do no intersect
#' with other nodes in the `sf` LINESTRING object. If `lines_input` contains geometries
#' of type MULTILINESTRING, they are cast to LINESTRING before computing.
#' @export
polyg_leaf_nodes <- function(lines_input, parallel = FALSE, n_cores = NULL, verbose = FALSE) {
  if (any(!unique(sf::st_geometry_type(lines_input)) %in% c("LINESTRING", "MULTILINESTRING"))) {
    stop("Lines input should be LINESTRING or MULTILINESTRING")
  }
  if (any(unique(sf::st_geometry_type(lines_input)) %in% c("MULTILINESTRING"))) {
    lines_input <- sf::st_cast(sf::st_cast(lines_input, "MULTILINESTRING", warn = FALSE), "LINESTRING", warn = FALSE)
  }
  if (parallel == TRUE & is.null(n_cores)) {
    stop("Please select number of cores when computing in parallel")
  }
  start_time <- Sys.time()
  if (verbose == TRUE) {
    message("Computing leaf nodes")
  }
  endpoints <- c(lwgeom::st_startpoint(lines_input), lwgeom::st_endpoint(lines_input))
  if (parallel == TRUE) {
    split_vector <- rep(1:n_cores, each = nrow(lines_input) / n_cores, length.out = nrow(lines_input))
    split_results <- future.apply::future_lapply(
      split(lines_input, split_vector),
      function(x) sf::st_collection_extract(sf::st_intersection(x, lines_input), "POINT", warn = FALSE)
    )
    intersections <- do.call("rbind", split_results)
  } else {
    intersections <- sf::st_collection_extract(sf::st_intersection(lines_input), "POINT", warn = FALSE)
  }
  diff <- setdiff(sf::st_geometry(endpoints), sf::st_geometry(intersections))
  leaf_nodes <- sf::st_as_sf(sf::st_sfc(diff, crs = sf::st_crs(lines_input)))
  leaf_nodes <- rename_geometry(leaf_nodes, "geometry")
  end_time <- Sys.time()
  if (verbose == TRUE) {
    message(paste("Leaf nodes computed in", round(difftime(end_time, start_time, units = "mins"), 2), "minutes"))
  }
  return(leaf_nodes)
}
