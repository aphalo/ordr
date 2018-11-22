#' Restrict ordination data to the convex hulls of both matrix factors
#'
#' As used in a \strong{\link{ggplot2}} vignette, this stat layer restricts a
#' dataset with \code{x} and \code{y} variables to its convex hull. The biplot
#' extension restricts each matrix factor to its own hull.
#' 

#' @template ggbiplot-layers

#' @name ggbiplot-chull
#' @inheritParams ggplot2::layer
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}}.

#' @rdname ggbiplot-chull
#' @usage NULL
#' @export
StatChull <- ggproto(
  "StatChull", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales) {
    data[chull(data$x, data$y), , drop = FALSE]
  }
)

#' @rdname ggbiplot-chull
#' @export
stat_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA, 
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatChull,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggbiplot-chull
#' @usage NULL
#' @export
StatUChull <- ggproto(
  "StatUChull", StatChull,
  
  setup_data = setup_u_data
)

#' @rdname ggbiplot-chull
#' @usage NULL
#' @export
StatVChull <- ggproto(
  "StatVChull", StatChull,
  
  setup_data = setup_v_data
)

#' @rdname ggbiplot-chull
#' @export
stat_u_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatUChull,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggbiplot-chull
#' @export
stat_v_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVChull,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}