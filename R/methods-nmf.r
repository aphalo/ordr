#' @title Functionality for non-negative matrix factorization ('NMF') objects
#' 
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"NMF"` as returned by [NMF::nmf()].
#' 
#' @name methods-nmf
#' @include ord-tbl.r
#' @template param-methods
NULL

#' @rdname methods-nmf
#' @export
as_tbl_ord.NMF <- as_tbl_ord_default

#' @rdname methods-nmf
#' @export
reconstruct.NMF <- function(x) {
  x@fit@W %*% x@fit@H
}

#' @rdname methods-nmf
#' @export
recover_u.NMF <- function(x) x@fit@W

#' @rdname methods-nmf
#' @export
recover_v.NMF <- function(x) t(x@fit@H)

#' @rdname methods-nmf
#' @export
recover_coord.NMF <- function(x) {
  paste0("F", 1:ncol(x@fit@W))
}

#' @rdname methods-nmf
#' @export
augmentation_u.NMF <- function(x) {
  .name <- rownames(x@fit@W)
  if (is.null(.name)) {
    tibble_pole(nrow(x@fit@W))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-nmf
#' @export
augmentation_v.NMF <- function(x) {
  .name <- colnames(x@fit@H)
  if (is.null(.name)) {
    tibble_pole(ncol(x@fit@H))
  } else {
    tibble(.name = .name)
  }
}

#' @rdname methods-nmf
#' @export
augmentation_coord.NMF <- function(x) {
  tibble(
    .name = recover_coord.NMF(x)
  )
}
