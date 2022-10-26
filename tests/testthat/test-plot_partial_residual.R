set.seed(232)
test_that("partial_residual_plot works", {
  mod = lm(y~a + b + z + x, data=small)
  partial_residual_plot(y~x, lm_formula = y~x + b, data=small) %>%
    vdiffr::expect_doppelganger("prp", .)
  partial_residual_plot(y~x, model=mod, added_term=~x, data=small) %>%
    vdiffr::expect_doppelganger("prp with model and added term", .)
  partial_residual_plot(y~x, model=mod, added_term=~x, data=small, suppress_model=T, method="lm") %>%
    vdiffr::expect_doppelganger("prp with model suppressed + flexplot arguments", .)
})
