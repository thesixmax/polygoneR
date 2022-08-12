# **polygone:** manipulating polygon and linestring data in R

This package is an extension of [`sf`](https://github.com/r-spatial/sf) and [`lwgeom`](https://github.com/r-spatial/lwgeom) for spatial analysis in R. The aim is to simplify common tasks carried out on spatial data such as computing contiguities, determine leaf nodes and conditional merging of polygons.

## Installation

This package depends on the spatial packages [`sf`](https://github.com/r-spatial/sf) and [`lwgeom`](https://github.com/r-spatial/lwgeom). Additionally, [`future.apply`](https://github.com/HenrikBengtsson/future.apply) is optional for processing certain functions in parallel. It is recommended to install these packages first.

Execute the following to install the latest available version of `polygone`:

    library(devtools)
    install_github("thesixmax/polygone")

## List of functions

The following is the up to date list of functions available in the package. Please refer to the individual man pages for more information. Extended documentation with examples is work in progress.

-   `polyg_contiguity` Compute a sparse index list of contiguities for an sf POLYGON object.

-   `polyg_leaf_nodes`: Compute leaf nodes of an sf LINESTRING object.

-   `polyg_nearest_node`: Compute the minimum distance linestring from an sf POINT object to the nodes of an sf LINESTRING object.

-   `polyg_dissolve_enclosed`: Dissolve fully or partially enclosed polygons in a sf POLYGON object.

-   `polyg_merge_conditional`: Iterative merging of polygons in a sf POLYGON object according to a condition set by the user.

## Collaborators

urbanLAB [(Department of the Built Environment, Aalborg University)](https://www.en.build.aau.dk)

[Data Clinic (Two Sigma)](https://dataclinic.twosigma.com)

## Acknowledgement

This package is part of a research project which gratefully acknowledges financial support from:

[data.org](https://data.org)

[Department of the Built Environment, Aalborg University](https://www.en.build.aau.dk)
