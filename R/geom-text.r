#' Render text at ordinates
#' 

#' \code{geom_*_text} renders text, and \code{geom_*_label} labels, at the 
#' positions of the subjects or vectors. \code{geom_*_text_repel} and 
#' \code{geom_*_label_repel} invoke functionality from
#' \strong{\link[ggrepel]{ggrepel}}.
#' @template ggbiplot-layers

#' @section Aesthetics:
#' \code{geom_*_text}, \code{geom_*_label}, \code{geom_*_text_repel}, and
#' \code{geom_*_label_repel} understand the following aesthetics (required
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
#'   \item \code{lineheight}
#'   \item \code{size}
#' }
#' 

#' @name ggbiplot-text
#' @import ggplot2
#' @importFrom ggrepel GeomTextRepel GeomLabelRepel
#' @inheritParams ggplot2::layer
#' @template param-layer
#' @param parse,check_overlap,nudge_x,nudge_y See
#'   \code{\link[ggplot2]{geom_text}}.
#' @template param-matrix

#' @rdname ggbiplot-text
#' @export
geom_u_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0, nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  if (! missing(nudge_x) || !missing(nudge_y)) {
    if (! missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomText,
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

#' @rdname ggbiplot-text
#' @export
geom_v_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0, nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  if (! missing(nudge_x) || !missing(nudge_y)) {
    if (! missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomText,
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

#' @rdname ggbiplot-text
#' @export
geom_biplot_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "u",
  ...,
  parse = FALSE,
  nudge_x = 0, nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  if (! missing(nudge_x) || !missing(nudge_y)) {
    if (! missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomText,
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

#' @rdname ggbiplot-text
#' @export
geom_u_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_v_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_biplot_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "u",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_u_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_v_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_biplot_text_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "u",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomTextRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_u_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_v_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-text
#' @export
geom_biplot_label_repel <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "u",
  ...,
  parse = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomLabelRepel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}
