# **polygoneR:** manipulate polygon and linestring data in R

This package is collection of helper functions and new tools built on [`sf`](https://github.com/r-spatial/sf) for spatial analysis in R. The aim is to simplify common tasks carried out on spatial data such as computing angles, leaf nodes and lines to nearest node.

## Installation

This package depends on the spatial package [`sf`](https://github.com/r-spatial/sf). It is recommended to install this package first.

Execute the following to install the latest available version of `polygoneR`:

```         
library(devtools)
install_github("thesixmax/polygoneR")
```

## List of functions

The following is the up to date list of functions available in the package. Please refer to the individual man pages for more information. Extended documentation, including examples, is work in progress.

| Function                  | Function type | Description                                                                                           |
|:-------------------|:-----------------|:-------------------------------|
| `polyg_angles`            | Helper        | Calculate the minimum and maximum angles between two sf LINESTRING objects.                           |
| `polyg_cast_substring`    | Helper        | Cast an sf object to substring.                                                                       |
| `polyg_contiguity`        | Helper        | Common contiguities of an sf POLYGON object.                                                          |
| `polyg_leaf_nodes`        | Helper        | Identify leaf nodes of an sf LINESTRING object.                                                       |
| `polyg_line_nearest_node` | Helper        | Calculate the minimum distance linestring from an sf POINT object to nodes of a sf LINESTRING object. |
| `polyg_rename_geom`       | Helper        | Rename the geometry column of an sf object.                                                           |

## Acknowledgements

The package is part of a research project which gratefully acknowledges financial support from [data.org](https://data.org) and [Department of the Built Environment, Aalborg University](https://www.en.build.aau.dk). The author thanks colleagues at urbanLAB [(Department of the Built Environment, Aalborg University)](https://www.en.build.aau.dk) and [Data Clinic (Two Sigma)](https://dataclinic.twosigma.com) for their input and suggestions for the initial build.
