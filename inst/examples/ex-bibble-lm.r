# reproduce Exhibit 2.3 in Greenacre (2010)

data(bioenv)
# standardize two predictors
bioenv <- dplyr::mutate(
  bioenv,
  x = (Depth - mean(Depth)) / sd(Depth),
  y = (Pollution - mean(Pollution)) / sd(Pollution)
)

# linear regression setup
(m <- lm(data = bioenv, formula = d ~ x + y))
(b <- as_bibble(m))
(d <- tidy(b))

# contributions of predictors to explained variance
print(coord_annot(b))

# basic linear regression biplot
gg <- ggbiplot(b, aes(x = x, y = y))
gg +
  geom_u_point() +
  geom_v_vector()
# same biplot using default matrices for each layer
gg +
  geom_biplot_point() +
  geom_biplot_vector()
# same biplot with isolines for sole axis
gg +
  geom_u_point() +
  geom_v_vector() +
  geom_v_isolines(ids = 1, by = 5)

# shade points by expected species prevalence and darken outliers
ggbiplot(d, aes(x = x, y = y, color = .fitted, alpha = .resid^2)) +
  theme_bw() +
  scale_color_distiller(type = "div", palette = 1) +
  scale_alpha_continuous(range = c(1/3, 1)) +
  geom_u_point() +
  geom_v_vector() +
  geom_v_isolines(ids = 1, by = 5)
