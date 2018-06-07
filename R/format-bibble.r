
format.bbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  
  # dimensional parameters
  uv_rows <- sapply(get_factor(x, "uv"), nrow)
  x_coords <- get_coord(x)
  n_coords <- length(x_coords)
  if (is.null(n)) {
    n <- ifelse(
      uv_rows > op.bibble$bibble.print_max,
      op.bibble$bibble.print_min,
      uv_rows
    )
  }
  width <- width %||% op.bibble$bibble.width %||% getOption("width")
  uv_extra <- set_names(rep(
    n_extra %||% op.bibble$bibble.max_extra_cols,
    length.out = 2
  ), c("u", "v"))
  
  # headers!
  header <- bbl_sum(x)
  coord_sum <- paste0(
    "# ", n_coords, " coordinate", ifelse(n_coords == 1, "", "s"), ": ",
    print_reps(x_coords)
  )
  
  # format U and V together first, then split
  fmts_coord <- format(select(
    rbind(
      as_tibble(get_u(x))[1:n[1], , drop = FALSE],
      as_tibble(get_v(x))[1:n[2], , drop = FALSE]
    ),
    1:min(n_coords, 3)
  ), n = sum(n), width = width)
  wh_rows <- which(stringr::str_detect(fmts_coord, "^ *[0-9]+ "))
  id_width <- diff(as.vector(unique(stringr::str_locate(
    fmts_coord,
    "^ *[0-9]+ "
  )[wh_rows, , drop = FALSE])))
  fmt_coord <- list(
    u = unname(c(
      tbl_sum(x)["u"],
      fmts_coord[2],
      stringr::str_pad("", nchar(fmts_coord[2])),
      fmts_coord[wh_rows[1:n[1]]]
    )),
    v = unname(c(
      tbl_sum(x)["v"],
      fmts_coord[2],
      stringr::str_pad("", nchar(fmts_coord[2])),
      str_replace(
        fmts_coord[wh_rows[n[1] + 1:n[2]]],
        "^ *[0-9]+ ",
        paste0(format(1:n[2], width = id_width), " ")
      )
    ))
  )
  coord_width <- unique(nchar(fmts_coord)[-1])
  
  uv_footer <- set_names(c("", ""), c("u", "v"))
  fmt_ann <- lapply(c("u", "v"), function(.matrix) {
    fmt <- factor_attr(x, .matrix)
    if (ncol(fmt) == 0) return("")
    fmt <- format(fmt, n = n[.matrix], width = width - coord_width - 7)
    # blank header
    fmt[1] <- paste(rep(" ", times = nchar(fmt[2])), collapse = "")
    # remove footer
    if (grepl("^#", fmt[length(fmt)])) {
      uv_footer[.matrix] <<- fmt[length(fmt)]
      fmt <- fmt[-length(fmt)]
    }
    # remove row indices
    fmt <- substring(
      fmt,
      nchar(gsub("(^[0-9]+ ).*$", "\\1", fmt[length(fmt)])) + 1
    )
    fmt
  })
  uv_footer <- stringr::str_replace_all(
    uv_footer,
    "#",
    paste0("#", stringr::str_pad("", coord_width + ifelse(n_coords > 3, 4, 0)))
  )
  
  seps <- if (n_coords > 3) c("    ", " ...") else c("", "")
  fmt_seps <- mapply(
    function(x, y) {
      sep_dots_rows <- ceiling(c(2, (y - 2) / 2 + 2))
      c(paste(rep(" ", times = max(0, x)), collapse = ""),
        paste0(ifelse(2:y %in% sep_dots_rows, seps[2], seps[1]), " | "))
    },
    x = 3 + nchar(seps) + coord_width -
      sapply(fmt_coord, function(z) nchar(z[1])),
    y = sapply(fmt_coord, length),
    SIMPLIFY = FALSE
  )
  uv_fmt <- mapply(
    paste0,
    fmt_coord, fmt_seps, fmt_ann,
    SIMPLIFY = FALSE
  )
  uv_fmt <- mapply(
    function(fmt, footer) if (footer == "") fmt else c(fmt, footer),
    fmt = uv_fmt,
    footer = uv_footer,
    SIMPLIFY = FALSE
  )
  
  c(header, coord_sum, "# ", uv_fmt[[1]], "# ", uv_fmt[[2]])
}

print.bbl <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  fmt <- format(x, ..., n = n, width = width, n_extra = n_extra)
  cat(paste0(fmt, collapse = "\n"), sep = "")
  invisible(x)
}

# settings

# this trick is borrowed from **tibble**
op.bibble <- list(
  bibble.print_max = 10L,
  bibble.print_min = 5L,
  bibble.width = NULL,
  bibble.coord_prop = .6,
  bibble.max_extra_cols = 60
)

bbl_sum <- function(x) UseMethod("bbl_sum")
bbl_sum.bbl <- function(x) {
  prev_class <- setdiff(class(x), "bbl")[1]
  bbl_descr <- if (!is.null(prev_class) && prev_class != "list") {
    paste0("# A '", prev_class, "' bibble")
  } else {
    paste0("# A bibble")
  }
  n_u <- nrow(get_u(x))
  n_v <- nrow(get_v(x))
  n_c <- length(get_coord(x))
  paste0(bbl_descr, ": (", n_u, " x ", n_c, ") x (", n_v, " x ", n_c, ")'")
}
tbl_sum.bbl <- function(x) {
  m <- sapply(get_factor(x, "uv"), dim)
  a <- sapply(factor_attr(x, "uv"), dim)
  set_names(paste0(
    "# ", c("U", "V"),
    ": [ ", m[1, ], " x ", m[2, ], " | ", a[2, ], " ]"
  ), c("u", "v"))
}

print_reps <- function(x) {
  x <- as.character(x)
  switch(
    min(length(x), 4),
    "1" = x,
    "2" = paste(x, collapse = " and "),
    "3" = paste(x, collapse = ", "),
    "4" = paste0(paste(x[1:2], collapse = ", "), ", ..., ", x[length(x)])
  )
}
