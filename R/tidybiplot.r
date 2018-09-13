#' @title \strong{tidybiplot} package
#'
#' @description Handling, manipulating, and visualizing ordination models in a
#'   \strong{tidyverse} framework
#'

#' This package is designed to integrate ordination analysis and biplot
#' visualization into a tidyverse workflow. It is inspired in particular by the
#' \strong{tidyverse} extensions \strong{ggbiplot} and \strong{tidygraph}.
#'
#' The package consists in several modules:

#' \itemize{

#'   \item the \code{\link{tbl_ord}} class, a wrapper for various ordination
#'   object classes

#'   \item extracting \code{\link{augmentation}} for the factors of an
#'   ordination

#'   \item using \code{\link{dplyr-verbs}} to add \code{\link{annotation}} to
#'   the factors of an ordination

#'   \item manipulating the coordinates of an ordination via
#'   \code{\link{alignment}} with other objects and \code{\link{conference}} of
#'   inertia

#'   \item methods of the above generics for several widely-used ordination
#'   object classes

#'   \item convenient \code{\link{formatting}} of ordination objects that
#'   respects the above manipulations

#'   \item \code{\link{ggbiplot}}, a \strong{ggplot2} extension for rendering
#'   biplots, including several plot layers, integrated with the \code{tbl_ord}
#'   class

#' }

#' @docType package
#' @name tidybiplot
NULL
