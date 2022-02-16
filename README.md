# **polygone:** functions for manipulating polygon and linestring data in R

This package is an extension of [`sf`](https://github.com/r-spatial/sf) and [`lwgeom`](https://github.com/r-spatial/lwgeom) for spatial analysis in R. The aim is to simplify common tasks carried out on spatial data such as computing contiguities, determine leaf nodes and conditional merging of polygons.

## Installation

This package depends on the spatial packages [`sf`](https://github.com/r-spatial/sf) and [`lwgeom`](https://github.com/r-spatial/lwgeom) and `dplyr` for data manipulations. Additionally, [`future.apply`](https://github.com/HenrikBengtsson/future.apply) is optional for processing certain functions in parallel. It is recommended to install these packages first.

Execute the following to install the latest available version of `polygone`:

    library(devtools)
    instal_github("thesixmax/polygone")

## List of functions

The following is the up to date list of functions available in the package. Please refer to the individual man pages for more information. Extended documentation with examples is work in progress.

-   `st_queen`/`st_rook`/`st_bishop` Compute a sparse index list of contiguities for an `sf` POLYGON object.

-   `st_leaf_nodes`: Compute leaf nodes of an `sf` LINESTRING object.

-   `st_nearest_node`: Compute the minimum distance linestring from an `sf` POINT object to the nodes of an `sf` LINESTRING object.

## Acknowledgement

This package is part of a research project which gratefully acknowledges financial support from:

[data.org](https://data.org)

[Department of the Built Environment, Aalborg University](https://www.en.build.aau.dk)
