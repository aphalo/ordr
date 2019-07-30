#' @title Render isolines for a subject or variable
#'
#' @description `geom_*_isoline()` renders isolines for a specified subject or
#'   variable.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_isoline()` understands the following aesthetics
#' (required aesthetics are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name geom-biplot-isoline
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams stat_rule
#' @template param-geom
#' @template param-matrix
# @example inst/examples/mtcars-lm-isoline.r
# @example inst/examples/bioenv-lm-isoline.r
# @example inst/examples/bioenv-glm-isoline.r
NULL

#' @rdname geom-biplot-isoline
#' @usage NULL
#' @export
GeomIsoline <- ggproto(
  "GeomIsoline", GeomAbline,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", size = .5, linetype = "dashed", alpha = .5
  ),
  
  setup_data = function(data, panel_params) {
    
    data
  },
  
  draw_panel = function(data, panel_params, coord) {
    print("draw_panel")
    print(data)
    
    # -+- ensure that vertical lines are rendered correctly -+-
    GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)

#' @rdname geom-biplot-isoline
#' @export
geom_isoline <- function(
  mapping = NULL, data = NULL, stat = "rule", position = "identity",
  #axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
  #origin = ! is.null(family_fun),
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIsoline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      #axes = axes,
      #calibrate = calibrate,
      #family_fun = family_fun,
      #by = by,
      #origin = origin,
      ...
    )
  )
}

#' @rdname geom-biplot-isoline
#' @export
geom_u_isoline <- function(
  mapping = NULL, data = NULL, stat = "rule", position = "identity",
  #axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
  #origin = ! is.null(family_fun),
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomIsoline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      #axes = axes,
      #calibrate = calibrate,
      #family_fun = family_fun,
      #by = by,
      #origin = origin,
      ...
    )
  )
}

#' @rdname geom-biplot-isoline
#' @export
geom_v_isoline <- function(
  mapping = NULL, data = NULL, stat = "rule", position = "identity",
  #axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
  #origin = ! is.null(family_fun),
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomIsoline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      #axes = axes,
      #calibrate = calibrate,
      #family_fun = family_fun,
      #by = by,
      #origin = origin,
      ...
    )
  )
}

#' @rdname geom-biplot-isoline
#' @export
geom_biplot_isoline <- function(
  mapping = NULL, data = NULL, stat = "rule", position = "identity",
  .matrix = "v",# axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
  #origin = ! is.null(family_fun),
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomIsoline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      #axes = axes,
      #calibrate = calibrate,
      #family_fun = family_fun,
      #by = by,
      #origin = origin,
      ...
    )
  )
}
