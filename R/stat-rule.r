#' @title Compute regular positions and orientations along ordination axes
#'
#' @description This stat layer produces the positions of ordinates or isolines
#'   along ordination axes with their associated slopes (perpendicular to the
#'   axes).
#'   

#' @template biplot-layers

#' @name stat-biplot-rule
#' @inheritParams ggplot2::layer
#' @param axes Indices of axes for which to render elements.
#' @param calibrate Logical; whether to calibrate axis scales for inner product
#'   interpretability.
#' @param family_fun A family function, or a character string naming one, to
#'   transform the values along the axis at which to render elements.
#' @param by Interval length between elements, in the units of the ordination.
#' @param origin Logical; whether to include elements located at the origin.
#' @param ... Additional parameters passed to [base::format()].
#' @template param-stat
# @example
NULL

family_arg <- function(family_fun) {
  if (! is.null(family_fun)) {
    if (is.character(family_fun)) {
      family_fun <- get(family_fun, mode = "function", envir = parent.frame())
    }
    if (is.function(family_fun)) {
      family_fun <- family_fun()
    }
  }
  family_fun
}

setup_data_rule <- function(data, params, .matrix = NA) {
  print("setup_data_rule")
  print(data)
  print(params)
  # (only sees explicitly passed parameters)
  
  if (! is.na(.matrix)) {
    data <- setup_biplot_data(data, params, .matrix)
  }
  
  data <- data[params$axes, , drop = FALSE]
  
  # slopes
  data$slope <- data$y / data$x
  
  # change coordinates to avoid affecting plotting window
  #data <- transform(data, xunit = x, yunit = y)
  #data$x <- NULL
  #data$y <- NULL
  
  data
}

#' @rdname stat-biplot-rule
#' @usage NULL
#' @export
StatRule <- ggproto(
  "StatRule", Stat,
  
  required_aes = c("x", "y"),
  
  setup_params = function(data, params) {
    
    # by default, render elements for all axes
    if (is.null(params$axes)) params$axes <- 1:nrow(data)
    
    params
  },
  
  setup_data = setup_data_rule,
  
  compute_group = function(
    data, scales,
    axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
    origin = ! is.null(family_fun), ...
  ) {
    print("compute_group")
    print(data)
    save(data, scales, axes, calibrate, family_fun, by, origin,
         file = "temp.rda")
    load("temp.rda")
    
    # ensure intercept column (zero is appropriate for null family)
    if (calibrate) {
      if (! "intercept" %in% names(data)) {
        data$intercept <- 0
        if (! is.null(family_fun)) {
          warning("No `intercept` aesthetic provided; it has been set to zero.")
        }
      }
    } else {
      if ("intercept" %in% names(data)) {
        warning("Axis is not calibrated, so `intercept` will be ignored.")
      }
      if (! is.null(family_fun)) {
        warning("Axis is not calibrated, so `family_fun` will be ignored.")
      }
    }
    
    if (calibrate) {
      # process distribution parameter
      family_fun <- family_arg(family_fun)
      # calibrate axis scales
      data <- transform(
        data,
        xunit = x / (x^2 + y^2),
        yunit = y / (x^2 + y^2)
      )
    }
    data$x <- NULL
    data$y <- NULL
    data <- transform(data, ssunit = xunit^2 + yunit^2)
    # range of (calibrated) unit
    ranges <- apply(data[, c("xunit", "yunit")], 2, range)
    
    # window boundaries for axis positions
    data <- transform(
      data,
      winxmin = ifelse(xunit > 0, ranges[1, "xunit"], ranges[2, "xunit"]),
      winxmax = ifelse(xunit > 0, ranges[2, "xunit"], ranges[1, "xunit"]),
      winymin = ifelse(yunit > 0, ranges[1, "yunit"], ranges[2, "yunit"]),
      winymax = ifelse(yunit > 0, ranges[2, "yunit"], ranges[1, "yunit"])
    )
    
    # extreme positions, in axis units
    data <- transform(
      data,
      unitmin = (winxmin * xunit + winymin * yunit) / ssunit,
      unitmax = (winxmax * xunit + winymax * yunit) / ssunit
    )
    data$ssunit <- NULL
    data$winxmin <- NULL
    data$winxmax <- NULL
    data$winymin <- NULL
    data$winymax <- NULL
    
    # calibrate axis range according to intercept and family
    if (calibrate) {
      ran_vars <- c("unitmin", "unitmax")
      data[, ran_vars] <- data[, ran_vars] + data$intercept
      if (! is.null(family_fun)) {
        data[, ran_vars] <- family_fun$linkinv(as.matrix(data[, ran_vars]))
      }
    }
    
    # element units; by default, use Wilkinson's breaks algorithm
    if (is.null(by)) {
      bys <- lapply(1:nrow(data), function(i) {
        labeling::extended(data$unitmin[i], data$unitmax[i], 6)
      })
    } else {
      if (length(by) == 1) by <- rep(by, nrow(data))
      bys <- lapply(1:nrow(data), function(i) {
        floor(data$unitmin[i] / by[i]):ceiling(data$unitmax[i] / by[i]) * by[i]
      })
    }
    data <- data[rep(1:nrow(data), sapply(bys, length)), , drop = FALSE]
    data$units <- unlist(bys)
    data$unitmin <- NULL
    data$unitmax <- NULL
    # exclude origin
    if (! origin) data <- subset(data, units != 0)
    # text strings
    data <- transform(data, label = format(units, ...))
    
    # un-calibrate axis units according to intercept and family
    if (calibrate) {
      unit_vars <- c("units")
      if (! is.null(family_fun)) {
        data[, unit_vars] <- family_fun$linkfun(as.matrix(data[, unit_vars]))
      }
      data[, unit_vars] <- data[, unit_vars] - data$intercept
    }
    
    # axis positions
    data <- transform(
      data,
      x = units * xunit,
      y = units * yunit
    )
    data$xunit <- NULL
    data$yunit <- NULL
    data$units <- NULL
    
    # orientation aesthetics
    data <- transform(data, slope = - 1 / slope)
    data <- transform(data, intercept = y - slope * x)
    
    data
  }
)

#' @rdname stat-biplot-rule
#' @export
stat_rule <- function(
  mapping = NULL, data = NULL, geom = "isoline", position = "identity",
  axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
  origin = ! is.null(family_fun),
  ...,
  show.legend = NA, 
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRule,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      axes = axes,
      calibrate = calibrate,
      family_fun = family_fun,
      by = by,
      origin = origin,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname stat-biplot-rule
#' @usage NULL
#' @export
StatURule <- ggproto(
  "StatURule", StatRule,
  
  setup_data = function(data, params) {
    setup_data_rule(data, params, .matrix = "u")
  }
)

#' @rdname stat-biplot-rule
#' @usage NULL
#' @export
StatVRule <- ggproto(
  "StatVRule", StatRule,
  
  setup_data = function(data, params) {
    setup_data_rule(data, params, .matrix = "v")
  }
)

#' @rdname stat-biplot-rule
#' @export
stat_u_rule <- function(
  mapping = NULL, data = NULL, geom = "isoline", position = "identity",
  axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
  origin = ! is.null(family_fun),
  ...,
  show.legend = NA, 
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatURule,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      axes = axes,
      calibrate = calibrate,
      family_fun = family_fun,
      by = by,
      origin = origin,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname stat-biplot-rule
#' @export
stat_v_rule <- function(
  mapping = NULL, data = NULL, geom = "isoline", position = "identity",
  axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
  origin = ! is.null(family_fun),
  ...,
  show.legend = NA, 
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVRule,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      axes = axes,
      calibrate = calibrate,
      family_fun = family_fun,
      by = by,
      origin = origin,
      na.rm = FALSE,
      ...
    )
  )
}
