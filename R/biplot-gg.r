#' @title Biplots following the grammar of graphics
#' 
#' @description Build a biplot visualization from ordination data wrapped as a
#' `tbl_ord` object.
#' 

#' @details
#'
#' `ggbiplot()` produces a [ggplot][ggplot2::ggplot] object from a `tbl_ord`
#' object `ordination`. The baseline object is the default unadorned
#' `"ggplot"`-class object `p` with the following differences from what
#' [ggplot2::ggplot()] returns:
#' 
#' - `p$mapping` is augmented with `.matrix = .matrix`, which expects either
#' `.matrix = "u"` or `.matrix = "v"` from the biplot.

#' - `p$coordinates` is defaulted to [ggplot2::coord_equal()] in order to
#' faithfully render the geometry of an ordination.

#' - When `x` (`y`) is mapped to a coordinate of `ordination`, and if
#' `axis.percents` is `TRUE`, then `p$labels$x` (`p$labels$y`) defaults to the
#' coordinate name concatenated with the percentage of [inertia][conference]
#' captured by this coordinate.

#' - `p` is assigned the class `"ggbiplot"` in addition to `"ggplot"`. This
#' serves no functional purpose currently.
#' 

#' Furthermore, the user may feed single integer values to the `x` and `y`
#' aesthetics, which will be interpreted as the corresponding coordinates in the
#' ordination.
#'
#' If `sec.axes` is `TRUE`, then secondary `x` and `y` axes are added internally
#' using `scale_*_continuous()`. (**NB:** These will be overwritten by
#' additional position scales; a more stable implementation is needed.) When
#' including gridlines, set `scale.factor` to a ratio of small integers, up to
#' powers of 10 (e.g. \eqn{5} or \eqn{\frac{2}{3}\times 10^3}{2/3 * 1e3}), to
#' make secondary axis coordinates easier to read.
#'
#' `ord_aes()` is a convenience function that generates a full-rank set of
#' coordinate aesthetics `.coord1`, `.coord2`, etc. mapped to the shared
#' coordinates of the ordination object, along with any additional aesthetics
#' that are processed internally by [ggplot2::aes()].
#' 

#' @template ggbiplot-layers

#' @name ggbiplot
#' @import ggplot2
#' @param ordination A `[tbl_ord]`.
#' @param mapping List of default aesthetic mappings to use for the biplot. The
#'   default assigns the first two coordinates to the aesthetics `x` and `y`.
#'   Other assignments must be supplied in each layer added to the plot.
#' @param axis.percents Whether to concatenate default axis labels with inertia
#'   percentages.
#' @param sec.axes Matrix factor character (`"u"` or `"v"`) to specify a
#'   secondary set of axes.
#' @param scale.factor Numeric value used to scale the secondary axes against
#'   the primary axes if `sec.axes` is specified.
#' @param scale_u,scale_v Either the character name of a numeric variable in
#'   `get_*(ordination)` or a numeric vector of length
#'   `nrow(get_*(ordination))`, used to scale the coordinates of \eqn{U} or
#'   \eqn{V}, respectively.
#' @param ... Additional arguments passed to [ggplot2::ggplot()] or to
#'   [ggplot2::aes()].
#' @example inst/examples/mtcars-lm-isolines.r
#' @example inst/examples/iris-princomp-sec.r
#' @example inst/examples/finches-lpca-sec.r
#' @seealso [ggplot2::ggplot2()]

#' @rdname ggbiplot
#' @export
ggbiplot <- function(
  ordination = NULL, mapping = aes(x = 1, y = 2),
  axis.percents = TRUE, sec.axes = NULL, scale.factor = NULL,
  scale_u = NULL, scale_v = NULL,
  ...
) {
  if (axis.percents) {
    # store inertia
    inertia <- recover_inertia(ordination)
    if (all(is.na(inertia))) {
      axis.percents <- FALSE
    }
  }
  
  # fortify `ordination` if necessary
  ordination <- fortify(ordination, include = "all")
  
  # augment `mapping`, if necessary, with default coordinates
  mapping <- ensure_xy_aes(ordination, mapping)
  
  # scale 'U' or 'V' as indicated by `scale_u` and `scale_v`
  if (! is.null(scale_u)) {
    ordination <- scale_ord(ordination, "u", mapping, scale_u)
  }
  if (! is.null(scale_v)) {
    ordination <- scale_ord(ordination, "v", mapping, scale_v)
  }
  
  # if `sec.axes` is specified, then fortify `ordination` and
  # scale the secondary axis coordinates to match the primary axis
  if (! is.null(sec.axes)) {
    
    sec.axes <- match_factor(sec.axes)
    if (! sec.axes %in% c("u", "v")) {
      stop("Select one matrix factor, 'u' or 'v', to scale to secondary axes.")
    }
    pri.axes <- setdiff(c("u", "v"), sec.axes)
    
    if (is.null(scale.factor)) {
      ps_lim <- lapply(c(pri.axes, sec.axes), function(.m) {
        apply(
          # recover coordinates stored as attribute during `fortify()`
          ordination[ordination$.matrix == .m, get_coord(ordination)],
          2, function(x) c(min(x), max(x))
        )
      })
      scale.factor <- min(ps_lim[[1]] / ps_lim[[2]])
    }
    
    ordination <- dplyr::mutate_at(
      ordination,
      dplyr::vars(get_coord(ordination)),
      dplyr::funs(ifelse(ordination$.matrix == sec.axes, . * scale.factor, .))
    )
    
  }
  
  # conventional `ggplot()` call
  p <- ggplot(
    data = ordination,
    mapping = mapping,
    environment = parent.frame(),
    ...
  )
  # .matrix aesthetic indicating whether to plot cases or variables
  .matrix_aes <- list(.matrix = rlang::quo(!! rlang::sym(".matrix")))
  class(.matrix_aes) <- "uneval"
  p$mapping <- c(p$mapping, .matrix_aes)
  
  # if `sec.axes` is specified, then add secondary axes
  if (! is.null(sec.axes)) {
    # -+-THIS APPROACH IS VULNERABLE TO DOWNSTREAM `x` AND `y` SCALES-+-
    p <- p + scale_x_continuous(sec.axis = sec_axis(~ . / scale.factor))
    p <- p + scale_y_continuous(sec.axis = sec_axis(~ . / scale.factor))
  }
  
  # synchronize the scales of the axes
  p$coordinates <- coord_equal()
  
  # assign default axis labels
  if (axis.percents) {
    xy <- match(sapply(mapping, all.vars), get_coord(ordination))
    xy_aes <- get_coord(ordination)[xy]
    inertia_pct <- scales::percent(inertia / sum(inertia))
    if (! is.na(xy[1])) {
      p$labels$x <- paste0(xy_aes[1], " (", inertia_pct[xy[1]], ")")
    }
    if (! is.na(xy[2])) {
      p$labels$y <- paste0(xy_aes[2], " (", inertia_pct[xy[2]], ")")
    }
  }
  
  # add class label for potential future use
  class(p) <- c("ggbiplot", class(p))
  
  p
}

# interpret numerical x and y coordinates as coordinates;
# assume first two coordinates if none are provided
ensure_xy_aes <- function(ordination, mapping) {
  coords <- get_coord(ordination)
  coord_vars <- syms(coords)
  if (is.null(mapping$y)) {
    mapping <- c(aes(y = !! coord_vars[[2]]), mapping)
  } else {
    if (is.numeric(mapping$y) && length(mapping$y) == 1) {
      mapping <- c(
        aes(y = !! coord_vars[[mapping$y]]),
        mapping[setdiff(names(mapping), "y")]
      )
    }
  }
  if (is.null(mapping$x)) {
    mapping <- c(aes(x = !! coord_vars[[1]]), mapping)
  } else {
    if (is.numeric(mapping$x) && length(mapping$x) == 1) {
      mapping <- c(
        aes(x = !! coord_vars[[mapping$x]]),
        mapping[setdiff(names(mapping), "x")]
      )
    }
  }
  class(mapping) <- "uneval"
  mapping
}

# use `.m` to avoid conflict with '.matrix' column in `ordination`
scale_ord <- function(ordination, .m, mapping, scale) {
  if (is.character(scale)) scale <- ordination[[scale]]
  dplyr::mutate_at(
    ordination,
    dplyr::vars(stringr::str_remove(as.character(mapping[c("x", "y")]), "^~")),
    dplyr::funs(ifelse(ordination$.matrix == .m, . * scale, .))
  )
}

#' @rdname ggbiplot
#' @export
ord_aes <- function(ordination, ...) {
  # process all coordinate aesthetics
  ord_aes <- lapply(
    get_coord(ordination),
    function(nm) rlang::quo(!! rlang::sym(nm))
  )
  names(ord_aes) <- paste0(".coord", seq_along(ord_aes))
  # process other aesthetics
  other_aes <- aes(...)
  # concatenate aesthetics
  aes <- c(ord_aes, other_aes)
  class(aes) <- "uneval"
  aes
}
