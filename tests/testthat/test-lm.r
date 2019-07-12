library(ordr)
context("linear regression, classes 'lm', 'glm', and 'mlm'")

data(bioenv)
bioenv <- bioenv %>%
  dplyr::mutate(
    x = (Depth - mean(Depth)) / sd(Depth),
    y = (Pollution - mean(Pollution)) / sd(Pollution)
  )

fit_lm0 <- lm(data = bioenv, formula = d ~ x + y + 0)
fit_lm1 <- lm(data = bioenv, formula = d ~ x + y)
fit_lm2 <- lm(as.matrix(bioenv[, "d"]) ~
                as.matrix(bioenv[, c("x", "y")]))
fit_lm3 <- lm(as.matrix(bioenv[, "d"]) ~
                as.matrix(bioenv[, "x"]) +
                as.matrix(bioenv[, "y"]))
test_that("`as_tbl_ord()` coerces 'lm' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm0)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm2)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lm3)))
})

fit_mlm0 <- lm(as.matrix(bioenv[, c("a", "b", "c", "d", "e")]) ~
                 as.matrix(bioenv[, c("x", "y")]) + 0)
fit_mlm1 <- lm(as.matrix(bioenv[, c("a", "b", "c", "d", "e")]) ~
                 as.matrix(bioenv[, c("x", "y")]))
fit_mlm2 <- lm(as.matrix(bioenv[, c("a", "b", "c", "d", "e")]) ~
                 x + y, data = bioenv)
fit_mlm3 <- lm(as.matrix(bioenv[, c("a", "b", "c", "d", "e")]) ~
                 as.matrix(bioenv[, "x"]) +
                 as.matrix(bioenv[, "y"]))
test_that("`as_tbl_ord()` coerces 'mlm' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm0)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm2)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mlm3)))
})

fit_glm0 <- glm(data = bioenv, formula = d ~ x + y + 0, family = poisson)
fit_glm1 <- glm(data = bioenv, formula = d ~ x + y, family = poisson)
fit_glm2 <- glm(as.matrix(bioenv[, "d"]) ~
                  as.matrix(bioenv[, c("x", "y")]),
                family = poisson)
fit_glm3 <- glm(as.matrix(bioenv[, "d"]) ~
                  as.matrix(bioenv[, "x"]) +
                  as.matrix(bioenv[, "y"]),
                family = poisson)
test_that("`as_tbl_ord()` coerces 'glm' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm0)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm2)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_glm3)))
})
