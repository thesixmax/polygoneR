#' Iterative conditional merging of polygons in a `sf` POLYGON object
#' @description Iterative merging of polygons in a `sf` POLYGON object according to a condition set by the user.
#' @importFrom lwgeom st_perimeter
#' @importFrom sf st_geometry_type st_relate st_area st_intersection
#' st_make_valid st_length st_cast st_as_sf st_geometry
#' @param polygons_input object of class sf, sfc or sfg with geometry type POLYGON or MULTIPOLYGON.
#' @param contiguity character; either `"queen"` or `"rook`. Default is `"rook"`.
#' @param condition character; either `"area"` or `"compactness"`. Default is `"area"`
#' @param threshold numeric; the upper bound of condition for a polygon to be considered for merging.
#' @param breakpoint integer or numeric; number of polygons allowed to be above the threshold.
#' If set as an integer, the breakpoint refers to an exact number of polygons.
#' If set as numeric between 0 and 1, refers the to the ceiling of the proportion of the initial number of polygons.
#' @param verbose logical; should iterations and running number of polygons be printed? Default is `FALSE`.
#' @return An `sf` object of type POLYGON containing the merged set of polygons.
#' @details A polygon is always merged with the neighboring polygon with which it shares
#' the longest portions of its border. The merging is iterative in the sense that the polygon with the
#' smallest `condition` value is merged first after which the `condition` of remaining polygons
#' is calculated. This process is repeated until the number of polygons to be merged is less than
#' or equal to the `breakpoint` value.
#' Setting a `breakpoint` is in particular recommended when the condition is `"compactness"`.
#' An iteration does not guarantee that the resulting merged polygon has a larger `condition`
#' value than the initial polygon, which may lead to undesirable results when either the `condition`
#' value is small or no `breakpoint` is set.
#' @export
polyg_merge_conditional <- function(polygons_input, contiguity = "rook", condition = "area",
                                    threshold = NULL, breakpoint = NULL, verbose = FALSE) {
  if (any(!unique(sf::st_geometry_type(polygons_input)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Input should be POLYGON or MULTIPOLYGON")
  }
  if (!(contiguity %in% c("queen", "rook"))) {
    stop("Please select either queen or rook contiguity")
  }
  if (is.null(threshold)) {
    stop("Please set a threshold value")
  }
  if (is.null(breakpoint)) {
    stop("Please set a breakpoint value")
  }
  if (condition == "area") {
    condition_fun <- function(x) {
      as.numeric(sf::st_area(x))
    }
  }
  if (condition == "compactness") {
    condition_fun <- function(x) {
      4 * pi * (as.numeric(sf::st_area(x)) / as.numeric(lwgeom::st_perimeter(x))^2)
    }
  }
  poly_tmp <- polygons_input
  poly_tmp$id <- seq.int(nrow(poly_tmp))
  poly_tmp$condition_var <- condition_fun(poly_tmp)
  poly_tmp$self_neighbour <- ifelse(unlist(lapply(
    polyg_contiguity(poly_tmp,
      contiguity = contiguity,
      fill_empty = TRUE
    ),
    `[[`, 1
  )) == poly_tmp$id, 1, 0)
  poly_tmp$merge_var <- 0
  poly_tmp$merge_var <- ifelse(poly_tmp$self_neighbour == 1, 0,
    ifelse(poly_tmp$condition_var < threshold, 1, 0)
  )

  merge_count <- sum(poly_tmp$merge_var)
  breakpoint <- ifelse(breakpoint < 1 & breakpoint > 0,
    as.integer(ceiling(nrow(poly_tmp) * breakpoint)),
    as.integer(breakpoint)
  )
  if (verbose == TRUE) {
    message(paste("Expected polygons to merge:", merge_count - breakpoint))
  }
  iter <- 0
  while (merge_count > breakpoint) {
    iter <- iter + 1
    if (verbose == TRUE) {
      message(paste0("Iteration: ", iter, ". ", "Number of polygons: ", nrow(poly_tmp)))
    }
    merge_poly <- poly_tmp[poly_tmp$merge_var == 1, ]
    merge_poly <- merge_poly[merge_poly$condition_var == min(merge_poly$condition_var), ][1, ]
    merge_id <- merge_poly$id[1]
    neighbours <- polyg_contiguity(poly_tmp, contiguity = contiguity, fill_empty = TRUE)[[merge_id]]
    l_lengths <- vector()
    lines <- list()
    for (i in 1:length(neighbours)) {
      lines[[i]] <- sf::st_intersection(poly_tmp[merge_id, ], poly_tmp[neighbours[i], ])
      lines[[i]] <- sf::st_make_valid(sf::st_cast(lines[[i]], warn = FALSE))
      l_lengths[i] <- sf::st_length(lines[[i]]$geometry)
    }
    union_id <- neighbours[which.max(l_lengths)]
    union_poly <- poly_tmp[poly_tmp$id %in% c(merge_id, union_id), ]
    union_poly <- sf::st_as_sf(sf::st_union(union_poly))
    union_poly <- rename_geometry(union_poly, "geometry")
    poly_bind <- poly_tmp[!(poly_tmp$id %in% c(merge_poly$id, union_id)), ]
    poly_bind <- rbind(poly_bind["geometry"], union_poly["geometry"])
    poly_tmp <- sf::st_make_valid(poly_bind)
    poly_tmp$id <- seq.int(nrow(poly_tmp))
    poly_tmp$condition_var <- condition_fun(poly_tmp)
    poly_tmp$self_neighbour <- ifelse(unlist(lapply(
      polyg_contiguity(poly_tmp,
        contiguity = contiguity,
        fill_empty = TRUE
      ),
      `[[`, 1
    )) == poly_tmp$id, 1, 0)
    poly_tmp$merge_var <- 0
    poly_tmp$merge_var <- ifelse(poly_tmp$self_neighbour == 1, 0,
      ifelse(poly_tmp$condition_var < threshold, 1, 0)
    )
    merge_count <- sum(poly_tmp$merge_var)
  }
  if (verbose == TRUE) {
    message(paste("Merging complete. Final number of polygons:", nrow(poly_tmp)))
  }
  return(poly_tmp)
}
