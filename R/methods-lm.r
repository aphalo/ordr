#' @title Functionality for linear model objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"lm"`, `"glm"`, and `"mlm"` as returned by [stats::lm()]
#'   and [stats::glm()].
#'
#' @name methods-lm
#' @include ord-tbl.r
#' @template param-methods
#' @template param-matrix
#' @template param-align
#' @example inst/examples/ex-lm.r
#' @example inst/examples/ex-glm.r

#' @importFrom stats model.frame influence cooks.distance predict

#' @rdname methods-lm
#' @export
as_tbl_ord.lm <- as_tbl_ord_default

#' @rdname methods-lm
#' @export
reconstruct.lm <- function(x) {
  pred_mat <- as.matrix(x$model[, -1, drop = FALSE])
  names_fun <- if (class(x)[1] == "lm") names else rownames
  if (names_fun(x$coefficients)[1] == "(Intercept)") {
    pred_mat <- cbind(`(Intercept)` = 1, pred_mat)
  }
  coef_mat <- as.matrix(x$coefficients)
  if (class(x)[1] != "mlm") colnames(coef_mat) <- names(x$model)[1]
  as.data.frame(pred_mat %*% coef_mat)
}

#' @rdname methods-lm
#' @export
recover_u.lm <- function(x) {
  .intercept_col <- if (names(x$coefficients)[1] == "(Intercept)") {
    .ic <- matrix(1L, nrow = nrow(x$model), ncol = 1)
    colnames(.ic) <- "(Intercept)"
    .ic
  } else matrix(NA_integer_, nrow = nrow(x$model), ncol = 0)
  .predictors <- as.matrix(model.frame(x)[, -1])
  res <- cbind(.intercept_col, .predictors)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_v.lm <- function(x) {
  res <- t(x$coefficients)
  dimnames(res) <- list(
    if (is.matrix(x$model[, 1])) colnames(x$model[, 1]) else names(x$model)[1],
    recover_coord(x)
  )
  res
}

#' @rdname methods-lm
#' @export
recover_coord.lm <- function(x) {
  .predictors <- x$model[, -1]
  if (is.matrix(.predictors)) {
    coord <- colnames(.predictors)
  } else {
    coord <- names(.predictors)
    mat_coord <- which(sapply(
      .predictors,
      function(y) is.matrix(y) && ! is.null(colnames(y))
    ))
    coord[mat_coord] <- unname(unlist(lapply(
      mat_coord,
      function(i) colnames(.predictors[, i])
    )))
  }
  if (names(x$coefficients)[1] == "(Intercept)") {
    coord <- c("(Intercept)", coord)
  }
  coord
}

#' @rdname methods-lm
#' @export
augmentation_u.lm <- function(x) {
  res <- tibble(.name = rownames(model.frame(x)))
  infl <- influence(x, do.coef = FALSE)
  # diagnostics
  res$.hat <- infl$hat
  res$.sigma <- infl$sigma
  res$.cooksd <- cooks.distance(x, infl = infl)
  # residuals
  res$.wt.res <- infl$wt.res
  # predictions
  pred <- as.data.frame(predict(x, se.fit = TRUE)[1:2])
  names(pred) <- paste0(".", names(pred))
  res <- bind_cols(res, pred)
  # augmentation!
  res
}

#' @rdname methods-lm
#' @export
augmentation_v.lm <- function(x) {
  .name <- if (is.matrix(x$model[, 1])) {
    colnames(x$model[, 1])
  } else {
    names(x$model)[1]
  }
  tibble(.name = .name)
}

#' @rdname methods-lm
#' @export
augmentation_coord.lm <- function(x) {
  summ <- as.data.frame(stats::coef(summary(x)))
  names(summ) <- c(".estimate", ".std.error", ".t.value", ".p.value")
  as_tibble(data.frame(
    .name = recover_coord(x),
    summ
  ))
}

#' @rdname methods-lm
#' @export
permute_to.lm <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get permutations
  p <- permutation_to(get_factor(as_tbl_ord(x), .matrix), y)
  # tag 'cmds' object with permutation
  x <- attribute_alignment(x, diag(1, nrow = x$rank)[, p, drop = FALSE])
  # return annotated object
  x
}

#' @rdname methods-lm
#' @export
augmentation_u.glm <- function(x) {
  res <- tibble(.name = rownames(model.frame(x)))
  # diagnostics
  infl <- influence(x, do.coef = FALSE)
  zero_wt <- as.numeric(x$weights != 0)
  res$.hat <- infl$hat * zero_wt
  res$.sigma <- infl$sigma * zero_wt
  res$.cooksd <- cooks.distance(x, infl = infl)
  # residuals
  res$.dev.res <- infl$dev.res
  res$.pear.res <- infl$pear.res
  # all predictions (because why not)
  for (tp in c("link", "response", "terms")) {
    pred <- as.data.frame(predict(x, type = tp, se.fit = TRUE)[1:2])
    names(pred) <- paste0(".", tp, ".", names(pred))
    res <- bind_cols(res, pred)
  }
  # augmentation!
  res
}

#' @rdname methods-lm
#' @export
recover_u.mlm <- function(x) {
  .intercept_col <- if (rownames(x$coefficients)[1] == "(Intercept)") {
    .ic <- matrix(1L, nrow = nrow(x$model), ncol = 1)
    colnames(.ic) <- "(Intercept)"
    .ic
  } else matrix(NA_integer_, nrow = nrow(x$model), ncol = 0)
  .predictors <- as.matrix(model.frame(x)[, -1])
  res <- cbind(.intercept_col, .predictors)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_v.mlm <- function(x) {
  res <- t(x$coefficients)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_coord.mlm <- function(x) {
  .predictors <- x$model[, -1]
  if (is.matrix(.predictors)) {
    coord <- colnames(.predictors)
  } else {
    coord <- names(.predictors)
    mat_coord <- which(sapply(
      .predictors,
      function(y) is.matrix(y) && ! is.null(colnames(y))
    ))
    coord[mat_coord] <- unname(unlist(lapply(
      mat_coord,
      function(i) colnames(.predictors[, i])
    )))
  }
  if (rownames(x$coefficients)[1] == "(Intercept)") {
    coord <- c("(Intercept)", coord)
  }
  coord
}

#' @rdname methods-lm
#' @export
augmentation_u.mlm <- function(x) {
  tibble(
    .name = rownames(model.frame(x))
  )
}

#' @rdname methods-lm
#' @export
augmentation_v.mlm <- function(x) {
  .name <- colnames(x$coefficients)
  if (is.null(.name)) {
    .name <- paste(names(x$model)[1], 1:ncol(x$model[, 1]), sep = ".")
  }
  tibble(.name = .name)
}

#' @rdname methods-lm
#' @export
augmentation_coord.mlm <- function(x) {
  # model summaries
  summs <- purrr::map_df(
    stats::coef(summary(x)),
    as_tibble,
    rownames = ".term",
    .id = ".response"
  )
  names(summs)[3:6] <- c(".estimate", ".std.error", ".t.value", ".p.value")
  summs$.response <- gsub("^Response ", "", summs$.response)
  res <- as_tibble(data.frame(.name = recover_coord(x), summs))
  # nest to coordinates
  tidyr::nest(res, -dplyr::one_of(".name", ".term"), .key = ".summary")
}
