#' @title Functionality for logistic PCA and logistic SVD objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"lpca"` and `"lsvd"` from the
#'   **[logisticPCA][logisticPCA::logisticPCA-package]** package. The signature
#'   functions [logisticPCA::logisticPCA()] and [logisticPCA::logisticSVD()] are
#'   masked with wrappers that add row and column names from the input matrix to
#'   the output matrices.
#'
#' @name methods-lpca
#' @include ord-tbl.r
#' @template param-methods
#' @template param-matrix
#' @template param-align
#' @example inst/examples/ex-lpca.r

#' @importFrom stats plogis

#' @rdname methods-lpca
#' @export
as_tbl_ord.lsvd <- as_tbl_ord_default

#' @rdname methods-lpca
#' @export
reconstruct.lsvd <- function(x) {
  round(plogis(x$A %*% t(x$B)), 0)
}

recover_uv_lsvd <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "A", v = "B")]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lpca
#' @export
recover_u.lsvd <- function(x) recover_uv_lsvd(x, "u")

#' @rdname methods-lpca
#' @export
recover_v.lsvd <- function(x) recover_uv_lsvd(x, "v")

#' @rdname methods-lpca
#' @export
recover_coord.lsvd <- function(x) paste0("LSC", 1:ncol(x$A))

#' @rdname methods-lpca
#' @export
augmentation_u.lsvd <- function(x) {
  tibble(
    .name = rownames(x$A)
  )
}

#' @rdname methods-lpca
#' @export
augmentation_v.lsvd <- function(x) {
  tibble(
    .name = rownames(x$B),
    .mu = x$mu
  )
}

#' @rdname methods-lpca
#' @export
augmentation_coord.lsvd <- function(x) {
  tibble(
    .name = recover_coord.lsvd(x)
  )
}

#' @rdname methods-lpca
#' @export
negate_to.lsvd <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(get_factor(as_tbl_ord(x), .matrix), y)
  # tag 'lsvd' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$B)))
  # return annotated object
  x
}

#' @rdname methods-lpca
#' @export
as_tbl_ord.lpca <- as_tbl_ord_default

recover_uv_lpca <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "PCs", v = "U")]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lpca
#' @export
recover_u.lpca <- function(x) recover_uv_lpca(x, "u")

#' @rdname methods-lpca
#' @export
recover_v.lpca <- function(x) recover_uv_lpca(x, "v")

#' @rdname methods-lpca
#' @export
recover_coord.lpca <- function(x) paste0("LPC", 1:ncol(x$U))

#' @rdname methods-lpca
#' @export
augmentation_u.lpca <- function(x) {
  .name <- rownames(x$PCs)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$PCs))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-lpca
#' @export
augmentation_v.lpca <- function(x) {
  .name <- rownames(x$U)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$U))
  } else {
    tibble(.name = .name)
  }
  res$.mu <- x$mu
  res
}

#' @rdname methods-lpca
#' @export
augmentation_coord.lpca <- function(x) {
  tibble(
    .name = recover_coord.lpca(x)
  )
}

#' @rdname methods-lpca
#' @export
negate_to.lpca <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(recover_factor(as_tbl_ord(x), .matrix), y)
  # tag 'lpca' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$U)))
  # return annotated object
  x
}

#' @rdname methods-lpca
#' @export
as_tbl_ord.clpca <- as_tbl_ord_default

#' @rdname methods-lpca
#' @export
recover_u.clpca <- function(x) recover_uv_lpca(x, "u")

#' @rdname methods-lpca
#' @export
recover_v.clpca <- function(x) recover_uv_lpca(x, "v")

#' @rdname methods-lpca
#' @export
recover_coord.clpca <- function(x) paste0("LPC", 1:ncol(x$U))

#' @rdname methods-lpca
#' @export
augmentation_u.clpca <- function(x) {
  .name <- rownames(x$PCs)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$PCs))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-lpca
#' @export
augmentation_v.clpca <- function(x) {
  .name <- rownames(x$U)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$U))
  } else {
    tibble(.name = .name)
  }
  res$.mu <- x$mu
  res
}

#' @rdname methods-lpca
#' @export
augmentation_coord.clpca <- function(x) {
  tibble(
    .name = recover_coord.clpca(x)
  )
}

#' @rdname methods-lpca
#' @export
negate_to.clpca <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(recover_factor(as_tbl_ord(x), .matrix), y)
  # tag 'lpca' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$U)))
  # return annotated object
  x
}
