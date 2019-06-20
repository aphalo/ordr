#' @title Compute normal confidence ellipses for ordination factors
#'
#' @description These ordination stats are adapted from
#'   [ggplot2::stat_ellipse()].
#'   

#' @template biplot-layers

#' @name stat-biplot-ellipse
#' @inheritParams ggplot2::stat_ellipse
#' @template param-stat
#' @example inst/examples/iris-prcomp-center-ellipse.r
NULL

#' @rdname stat-biplot-ellipse
#' @usage NULL
#' @export
StatUEllipse <- ggproto(
  "StatUEllipse", StatEllipse,
  
  setup_data = setup_u_data
)

#' @rdname stat-biplot-ellipse
#' @usage NULL
#' @export
StatVEllipse <- ggproto(
  "StatVEllipse", StatEllipse,
  
  setup_data = setup_v_data
)

#' @rdname stat-biplot-ellipse
#' @export
stat_u_ellipse <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  type = "t",
  level = 0.95,
  segments = 51
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatUEllipse,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...,
      type = type,
      level = level,
      segments = segments
    )
  )
}

#' @rdname stat-biplot-ellipse
#' @export
stat_v_ellipse <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  type = "t",
  level = 0.95,
  segments = 51
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVEllipse,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...,
      type = type,
      level = level,
      segments = segments
    )
  )
}
