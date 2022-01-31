# **polygone:** functions for manipulating polygon and linestring data in R

This package is a an extension of existing R packages such as [`sf`](https://github.com/r-spatial/sf) and [`lwgeom`](https://github.com/r-spatial/lwgeom). The aim is to simplify common tasks carried out on spatial data such as computing contiguities, determine leaf nodes and conditional merging of polygons.

## Installation

This package depends heavily on [`sf`](https://github.com/r-spatial/sf) and [`lwgeom`](https://github.com/r-spatial/lwgeom) and to some extent on [`igraph`](https://github.com/igraph/igraph). Optionally, the package relies on [`future.apply`](https://github.com/HenrikBengtsson/future.apply) for processing certain functions in parallel. It is recommended to install these packages first.

Run the following to install the latest available version of `polygone`:

    library(devtools)
    instal_github("thesixmax/polygone")

### On the usage of dplyr

\<TEXT HERE>

sf works well with dplyr, easier to read and rewrite code, bla bla.

## List of functions

The following is the up to date list of functions available in the package. Please refer to the individual man pages for more information. Extended documentation with examples is work in progress.

-   `st_queen`/`st_rook`/`st_bishop` Compute a sparse index list of rook contiguities for an sf POLYGON object.

-   `st_leaf_nodes`: Compute leaf nodes of an sf LINESTRING object.

-   `st_nearest_node`: Compute the minimum distance linestring from an sf POINT object to the nodes of an sf LINESTRING object.

## Acknowledgement

This package is part of a research project which gratefully acknowledges financial support from

[data.org](https://data.org)
