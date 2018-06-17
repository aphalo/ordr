#' A unified ordination object class
#' 
#' These functions wrap ordination objects in the \code{"bbl"} ("bibble") class,
#' create bibbles directly from matrices, and test for the class and basic
#' structure.
#' 

#' The \code{"bbl"} ("bibble") class wraps around a range of ordination classes,
#' making available a suite of ordination tools that specialize to each original
#' object class, including \code{\link{format}} and \code{\link{fortify}}, which
#' facilitate the \code{\link{print}} method and the \code{\link{ggbiplot}}
#' function.
#' 
#' No default method is provided for \code{as_bibble}, despite most defined 
#' methods being equivalent (simply adding \code{"bbl"} to the vector of object 
#' classes). This prevents objects for which other methods are not defined from 
#' being re-classed as bibbles.
#' 
#' The function \code{make_bibble} creates a bibble structured as a list of two 
#' matrices, \code{u} and \code{v}, which must have the same number of columns 
#' and the same column names.
#' 
#' \code{is_bibble} checks an object \code{x} for the \code{"bbl"} class and for
#' consistency between \code{get_coord(x)} and the columns of \code{get_u(x)} 
#' and \code{get_v(x)}, using the functions at \code{\link{bibble-factors}}.
#' 

#' @name bibble
#' @include bibble-utils.r
#' @importFrom tibble tibble is_tibble as_tibble
#' @param x An ordination object.
#' @param u,v Matrices to be used as factors of a bibble.
#' @param ... Additional elements of a custom bibble.
#'   

#' @rdname bibble
#' @export
as_bibble <- function(x) UseMethod("as_bibble")

#' @rdname bibble
#' @export
as_bibble.bbl <- function(x) x

#' @rdname bibble
#' @export
make_bibble <- function(u = NULL, v = NULL, ...) {
  if (!is.matrix(u) || !is.matrix(v) || ncol(u) == ncol(v)) {
    stop("`u` and `v` must be matrices having the same number of columns.")
  }
  if (!is.null(colnames(u)) & !is.null(colnames(v))) {
    if (any(colnames(u) != colnames(v))) {
      stop("`u` and `v` must have the same column names.")
    }
  }
  res <- list(u = u, v = v, ...)
  class(res) <- c("bbl", class(res))
  res
}

#' @rdname bibble
#' @export
is_bibble <- function(x) {
  if (!inherits(x, "bbl")) return(FALSE)
  if (!is.null(attr(x, "coord_rotation")) &&
      !is.matrix(attr(x, "coord_rotation")) &&
      !all(dim(attr(x, "coord_rotation") == rep(dim(x), 2)))) return(FALSE)
  if (!is.null(attr(x, "u_annot")) &&
      !is_tibble(attr(x, "u_annot"))) return(FALSE)
  if (!is.null(attr(x, "v_annot")) &&
      !is_tibble(attr(x, "v_annot"))) return(FALSE)
  if (is.null(get_coord(x)) ||
      is.null(get_u(x)) ||
      is.null(get_v(x))) return(FALSE)
  if (!all(get_coord(x) %in% colnames(get_u(x)))) return(FALSE)
  if (!all(get_coord(x) %in% colnames(get_v(x)))) return(FALSE)
  TRUE
}

#' @rdname bibble
#' @export
is.bibble <- is_bibble

#' @rdname bibble
#' @export
un_bibble <- function(x) {
  if (!is_bibble(x)) return(x)
  attr(x, "coord_rotation") <- NULL
  attr(x, "u_annot") <- NULL
  attr(x, "v_annot") <- NULL
  class(x) <- setdiff(class(x), "bbl")
  x
}
