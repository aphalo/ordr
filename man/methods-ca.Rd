% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-ca.r
\name{methods-ca}
\alias{methods-ca}
\alias{as_tbl_ord.ca}
\alias{recover_rows.ca}
\alias{recover_cols.ca}
\alias{recover_inertia.ca}
\alias{recover_conference.ca}
\alias{recover_coord.ca}
\alias{augmentation_rows.ca}
\alias{augmentation_cols.ca}
\alias{augmentation_coord.ca}
\title{Functionality for correspondence analysis ('ca') objects}
\usage{
\method{as_tbl_ord}{ca}(x)

\method{recover_rows}{ca}(x)

\method{recover_cols}{ca}(x)

\method{recover_inertia}{ca}(x)

\method{recover_conference}{ca}(x)

\method{recover_coord}{ca}(x)

\method{augmentation_rows}{ca}(x)

\method{augmentation_cols}{ca}(x)

\method{augmentation_coord}{ca}(x)
}
\arguments{
\item{x}{An ordination object.}
}
\description{
These methods extract data from, and attribute new data to,
objects of class \code{"ca"} from the \strong{\link[ca:ca]{ca}} package.
}
\examples{
# data frame of artificial employee smoking habits
class(ca::smoke)
print(ca::smoke)
# use correspondence analysis to construct row and column profiles
ca::smoke \%>\%
  ca::ca() \%>\%
  as_tbl_ord() \%>\%
  print() -> smoke_ca
# summarize ordination
glance(smoke_ca)
# recover row and column profiles
get_rows(smoke_ca)
get_cols(smoke_ca)
# augment profiles with names, masses, distances, and inertias
augment_ord(smoke_ca)
# summarize artificial coordinates
tidy(smoke_ca)
# fortification of artificial coordinates yields proportion of variance measure
fortify(smoke_ca, .matrix = "coord")
# scree plot of inertia
ggplot(smoke_ca, .matrix = "coord", aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Inertia")
# scree plot of proportion of variance (inertia)
ggplot(smoke_ca, .matrix = "coord", aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(smoke_ca)
# row-principal biplot
smoke_ca \%>\%
  confer_inertia("rowprincipal") \%>\%
  ggbiplot(aes(label = .name, color = .matrix, shape = .matrix)) +
  scale_color_manual(values = c("navyblue", "brown")) +
  scale_shape_manual(values = c(17L, 16L)) +
  theme_bw() + theme_biplot() +
  geom_origin() +
  geom_rows_point() +
  geom_rows_text_repel(show.legend = FALSE) +
  geom_cols_point() +
  geom_cols_text_repel(show.legend = FALSE) +
  labs(color = "Dimension", shape = "Dimension")
# row-principal biplot
smoke_ca \%>\%
  confer_inertia("colprincipal") \%>\%
  ggbiplot(aes(label = .name, color = .matrix, shape = .matrix)) +
  scale_color_manual(values = c("navyblue", "brown")) +
  scale_shape_manual(values = c(17L, 16L)) +
  theme_bw() + theme_biplot() +
  geom_origin() +
  geom_rows_point() +
  geom_rows_text_repel(show.legend = FALSE) +
  geom_cols_point() +
  geom_cols_text_repel(show.legend = FALSE) +
  labs(color = "Dimension", shape = "Dimension")
}
