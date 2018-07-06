#' Render text at ordinates radiating out from the origin
#' 

#' \code{geom_*_text_radiate} is adapted from \strong{link[ggbiplot]{ggbiplot}}.
#' It renders labels at specified positions and angles that radiate out from the
#' origin.
#' @template ggbiplot-layers

#' @section Aesthetics:
#' \code{geom_*_text_radiate} understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{label}}
#'   \item \code{alpha}
#'   \item \code{angle}
#'   \item \code{colour}
#'   \item \code{family}
#'   \item \code{fontface}
#'   \item \code{group}
#'   \item \code{hjust}
#'   \item \code{lineheight}
#'   \item \code{size}
#'   \item \code{vjust}
#' }
#' 

#' @name ggbiplot-text-radiate
#' @import ggplot2
#' @inheritParams ggbiplot-text

#' @rdname ggbiplot-text-radiate
#' @usage NULL
#' @export
GeomTextRadiate <- ggproto(
  "GeomTextRadiate", GeomText,
  
  draw_panel = function(
    data, panel_params, coord,
    parse = FALSE,
    na.rm = FALSE,
    check_overlap = FALSE
  ) {
    
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }
    data$hjust <- 0.5 + (data$hjust - 0.625 - 0.5) * sign(data$x)
    data$angle <- as.numeric(data$angle) + (180 / pi) * atan(data$y / data$x)
    
    ggplot2::GeomText$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      parse = parse,
      na.rm = na.rm,
      check_overlap = check_overlap
    )
  }
)

#' @rdname ggbiplot-text-radiate
#' @export
geom_u_text_radiate <- function(
  mapping = NULL, data = NULL, position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "u",
    geom = GeomTextRadiate,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text-radiate
#' @export
geom_v_text_radiate <- function(
  mapping = NULL, data = NULL, position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "v",
    geom = GeomTextRadiate,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text-radiate
#' @export
geom_biplot_text_radiate <- function(
  mapping = NULL, data = NULL, position = "identity",
  .matrix = "v",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = .matrix,
    geom = GeomTextRadiate,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

# not exported from *ggplot2*
#' @importFrom utils getFromNamespace
compute_just <- getFromNamespace("compute_just", "ggplot2")