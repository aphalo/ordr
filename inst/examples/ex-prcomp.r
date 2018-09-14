# principal components analysis (`prcomp`) examples

# calculate several `prcomp`s
x <- USPersonalExpenditure
p <- prcomp(x, center = FALSE, scale = FALSE)
p2 <- prcomp(x, center = TRUE, scale = FALSE)
p3 <- prcomp(x, center = TRUE, scale = TRUE)

# access the 'U' and 'V' matrices
recover_u(p)
recover_v(p)
recover_u(p2)
recover_v(p2)
recover_u(p3)
recover_v(p3)

# access the names of the artificial coordinates
recover_coord(p)
recover_coord(p2)
recover_coord(p3)

# check that the distances between the original and recovered values are small
range(as.matrix(x) - reconstruct(p))
range(as.matrix(x) - reconstruct(p2))
range(as.matrix(x) - reconstruct(p3))

# augment methods
augment_u(p)
augment_v(p)
augment_coord(p)
augment_u(p2)
augment_v(p2)
augment_coord(p2)
augment_u(p3)
augment_v(p3)
augment_coord(p3)

# wrap `p`, `p2`, and `p3` as 'tbl_ord' objects
b <- as_tbl_ord(p)
b2 <- as_tbl_ord(p2)
b3 <- as_tbl_ord(p3)

# data frames
fortify(b)
fortify(b2)
fortify(b3)

# unadjusted biplot of scores and loadings (using centered and scaled data)
gg <- ggbiplot(b3) +
  geom_u_point() +
  geom_v_vector()
gg
# add radiating text for loadings
gg + geom_v_text_radiate(aes(label = .name))

# "principal component biplot" of Gabriel
# (move `diag(sqrt(nrow(x)), dim(b3), dim(b3))` from U to V)
s <- sqrt(nrow(get_u(b3)))
ggbiplot(b3) +
  geom_u_point(aes(x = PC1 * s, y = PC2 * s)) +
  geom_v_vector(aes(x = PC1 / s, y = PC2 / s))

# effect of conferring inertia (using centered and scaled data)
get_conference(b3)
ggbiplot(b3, aes(label = .name)) +
  geom_u_point() +
  geom_u_text_repel() +
  geom_v_vector() +
  geom_v_text_radiate()
(b3_cov <- confer_inertia(b3, c(0, 1)))
ggbiplot(b3_cov, aes(label = .name)) +
  geom_u_point() +
  geom_u_text_repel() +
  geom_v_vector() +
  geom_v_text_radiate()
(b3_symm <- confer_inertia(b3, c(.5, .5)))
ggbiplot(b3_symm, aes(label = .name)) +
  geom_u_point() +
  geom_u_text_repel() +
  geom_v_vector() +
  geom_v_text_radiate()

# reproduce Exhibits 6.1 and 6.2 in Greenacre (2010)

# load data and store (square rots of) weights
data(country_attributes)
w_cas <- sqrt(1 / nrow(country_attributes))
w_var <- sqrt(1 / ncol(country_attributes))

# method 1: use `prcomp()` directly
# weights are missing from `country_attributes`, hence from inertia of `b`
(m <- prcomp(country_attributes))
(b <- as_tbl_ord(m))

# form biplot: move variable weights to case scores matrix
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_text(aes(x = PC1 * w_var, y = PC2 * w_var)) +
  geom_v_vector(aes(x = PC1 / w_var, y = PC2 / w_var)) +
  geom_v_text_repel(aes(x = PC1 / w_var, y = PC2 / w_var))

# covariance biplot: move case weights to variable loadings matrix
b_cov <- confer_inertia(b, c(0, 1))
ggbiplot(b_cov, aes(label = .name)) +
  theme_bw() +
  geom_u_text(aes(x = PC1 / w_cas, y = PC2 / w_cas)) +
  geom_v_vector(aes(x = PC1 * w_cas, y = PC2 * w_cas)) +
  geom_v_text_repel(aes(x = PC1 * w_cas, y = PC2 * w_cas))

# method 2: scale `x` by case and variable weights before SVD
x <- country_attributes
x <- diag(w_cas, nrow(x)) %*% x %*% diag(w_var, ncol(x))
dimnames(x) <- dimnames(country_attributes)
(m <- prcomp(x))
(b <- as_tbl_ord(m))

# form and covariance biplots: un-weight the left and right singular vectors
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_text(aes(x = PC1 / w_cas, y = PC2 / w_cas)) +
  geom_v_vector(aes(x = PC1 / w_var, y = PC2 / w_var)) +
  geom_v_text_repel(aes(x = PC1 / w_var, y = PC2 / w_var))
b_cov <- confer_inertia(b, c(0, 1))
ggbiplot(b_cov, aes(label = .name)) +
  theme_bw() +
  geom_u_text(aes(x = PC1 / w_cas, y = PC2 / w_cas)) +
  geom_v_vector(aes(x = PC1 / w_var, y = PC2 / w_var)) +
  geom_v_text_repel(aes(x = PC1 / w_var, y = PC2 / w_var))