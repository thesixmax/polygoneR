#' Dissolve enclosed polygons in a sf POLYGON object
#' @description Helper function for dissolving fully or partially enclosed polygons in a `sf` POLYGON object.
#' @importFrom lwgeom st_perimeter
#' @importFrom sf st_geometry_type st_relate st_geometry st_intersection st_make_valid
#' st_cast st_length st_as_sf st_union
#' @param polygons_input object of class sf, sfc or sfg with geometry type POLYGON or MULTIPOLYGON.
#' @param contiguity character; either `"rook"` or `"queen"`. Default is `"rook"`.
#' @param tolerance numeric; length in units to tolerate when determining if a polygon is enclosed.
#' Default is 0 (fully enclosed).
#' @return An sf object of type POLYGON with enclosed polygons dissolved.
#' @details An enclosed polygon is defined as having only one neighbor which shares all of the polygon border.
#' Setting tolerance > 0 allows for merging partially enclosed polygons, i.e. polygons where small portions
#' of the polygon border is not shared with any other polygon.
#' @export
polyg_dissolve_enclosed <- function(polygons_input, contiguity = "rook", tolerance = 0) {
  if (any(!unique(sf::st_geometry_type(polygons_input)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Input should be POLYGON or MULTIPOLYGON")
  }
  if (!(contiguity %in% c("queen", "rook"))) {
    stop("Please select either queen or rook contiguity")
  }
  poly_tmp <- polygons_input
  poly_tmp$id <- seq.int(nrow(poly_tmp))
  contiguity_list <- polyg_contiguity(poly_tmp, contiguity = contiguity, fill_empty = FALSE)
  merge_tmp <- list()
  union_tmp <- list()
  for (i in 1:length(contiguity_list)) {
    if (length(contiguity_list[[i]]) == 1) {
      merge_tmp[i] <- i
      union_tmp[i] <- contiguity_list[[i]]
    }
  }
  merge_id <- unlist(merge_tmp)
  union_id <- unlist(union_tmp)
  lines <- list()
  l_lengths <- vector()
  perimeters <- vector()
  enclosed <- vector()
  for (i in 1:length(merge_id)) {
    lines[[i]] <- sf::st_intersection(poly_tmp[merge_id[i], ], poly_tmp[union_id[i], ])
    lines[[i]] <- sf::st_make_valid(sf::st_cast(lines[[i]], warn = FALSE))
    l_lengths[i] <- sf::st_length(lines[[i]]$geometry)
    perimeters[i] <- lwgeom::st_perimeter(poly_tmp[merge_id[i], ])
    ifelse(perimeters[i] - l_lengths[i] <= tolerance,
      enclosed[i] <- TRUE,
      enclosed[i] <- FALSE
    )
  }
  merge_id <- merge_id[enclosed == TRUE]
  union_id <- union_id[enclosed == TRUE]
  union_unique <- unique(union_id)
  union_list <- list()
  for (i in 1:length(union_unique)) {
    union_list[[i]] <- c(union_unique[i], merge_id[union_id == union_unique[i]])
  }
  merge_list <- list()
  for (i in 1:length(union_list)) {
    poly_merge <- poly_tmp[poly_tmp$id %in% union_list[[i]], ]
    merge_list[[i]] <- sf::st_as_sf(sf::st_union(poly_merge))
  }
  merged_poly <- rename_geometry(Reduce(rbind, merge_list), "geometry")
  poly_bind <- poly_tmp[!(poly_tmp$id %in% c(merge_id, union_id)), ]
  poly_bind <- unique(sf::st_make_valid(rbind(poly_bind["geometry"], merged_poly["geometry"])))
  message(paste("Merging complete. Polygons dissolved:", nrow(polygons_input) - nrow(poly_bind)))
  return(poly_bind)
}
