#' @title Functionality for singular value decompositions
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"svd_ord"` returned by [svd_ord()].
#'
#' @name methods-svd
#' @include ord-tbl.r
#' @template param-methods
NULL

#' @rdname methods-svd
#' @export
as_tbl_ord.svd_ord <- as_tbl_ord_default

#' @rdname methods-svd
#' @export
recover_rows.svd_ord <- function(x) x[["u"]]

#' @rdname methods-svd
#' @export
recover_cols.svd_ord <- function(x) x[["v"]]

#' @rdname methods-svd
#' @export
recover_inertia.svd_ord <- function(x) x[["d"]]^2

#' @rdname methods-svd
#' @export
recover_coord.svd_ord <- function(x) colnames(x[["u"]])

#' @rdname methods-svd
#' @export
recover_conference.svd_ord <- function(x) {
  # `base::svd()` returns rotation matrices
  c(0, 0)
}

#' @rdname methods-svd
#' @export
augmentation_rows.svd_ord <- function(x) {
  .name <- rownames(x[["u"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["u"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augmentation_cols.svd_ord <- function(x) {
  .name <- rownames(x[["v"]])
  if (is.null(.name)) {
    tibble_pole(nrow(x[["v"]]))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-svd
#' @export
augmentation_coord.svd_ord <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .value = x[["d"]][1:ncol(x[["u"]])]
  )
}
