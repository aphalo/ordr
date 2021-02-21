#' @title Render polygons around ordinates
#' 

#' @description `geom_*_polygon()` renders polygons around the convex hulls of
#'   the positions of the subjects or vectors.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_polygon()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `fill`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name geom-biplot-polygon
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @template param-matrix
#' @example inst/examples/arrests-lra-polygon.r
NULL

#' @rdname geom-biplot-polygon
#' @export
geom_rows_polygon <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-polygon
#' @export
geom_cols_polygon <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-polygon
#' @export
geom_dims_polygon <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "rows",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
