# **polygone:** functions for manipulating polygon and linestring data in R

This package is a an extension of existing R packages such as *sf* and *lwgeom*. The aim is to simplify common tasks carried out on spatial data such as computing contiguities, determine leaf nodes and conditional merging of polygons.

## Installation

Simply run the following to install the latest developer version:

    library(devtools)
    instal_github("thesixmax/polygone")

## List of functions

The following is the up to date list of functions available in the package. Please refer to the individual man pages for more information. Extended documentation with examples is work in progress.

-   `st_rook`: Compute a sparse index list of rook contiguities for an sf POLYGON object.

-   `st_leaf_nodes`: Compute leaf nodes of an sf LINESTRING object.

-   `st_nearest_node`: Compute the minimum distance linestring from an sf POINT object to the nodes of an sf LINESTRING object.

## Acknowledgement

This package is part of a research project which gratefully acknowledges financial support from

[<img src="images/mwdy16M-_400x400.jpg" title="data.org" width="100"/>](https://data.org)
