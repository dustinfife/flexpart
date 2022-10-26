test_that("compute_mean_or_mode works", {
  expect_equal(compute_mean_or_mode(1:9), 5)
  expect_equal(compute_mean_or_mode(factor(letters[1:5])), "a")
  expect_equal(compute_mean_or_mode(letters[1:5]), "a")
  expect_equal(compute_mean_or_mode(c("b", "a", "b", "c")), "b")
})

test_that("return_matching_terms works", {
  model = lm(health~weight.loss + motivation * therapy.type, data=exercise_data)
  expect_true(all(return_matching_terms(~therapy.type*motivation, model)==c("motivation", "therapy.type", "motivation:therapy.type")))
  expect_true(all(return_matching_terms(~therapy.type+motivation, model)==c("therapy.type","motivation")))
  expect_true(all(return_matching_terms(~motivation*therapy.type, model)==c("motivation", "therapy.type", "motivation:therapy.type")))
})

test_that("reorder_interaction_terms works", {
  expect_true(reorder_interaction_terms("a:c:b")=="a:b:c")
  expect_true(reorder_interaction_terms("a")=="a")
})

test_that("partial_residual works", {
  mod = lm(health~motivation + therapy.type + muscle.gain, data=exercise_data)
  expect_equal(sum(partial_residual(mod, c("motivation", "therapy.type"))), -894.6831, tolerance=.001)
  expect_equal(round(as.numeric(partial_residual(mod, ~motivation)[1])*100), -358)
})

test_that("terms_to_modelmatrix works", {
  expect_true(ncol(terms_to_modelmatrix("motivation", exercise_data))==2)
  expect_true(ncol(terms_to_modelmatrix(~motivation, exercise_data))==2)
})

test_that("get_same_columns works", {
  d = data.frame(a=1:5*.1, b=1:5*.2, c=1:5*.4)
  d$ab = d$a*d$b
  e = d[,c("a", "b", "ab")]
  names(d)[3] = "ba"
  expect_true(duplicated(list(d$ab, get_same_columns(d,e)[,"ab"]))[2])
})

test_that("data_columns_as_list works", {
  d1 = data.frame(a=1:5, b=1:5, c=6:10)
  d2 = data.frame(a=1:5, d =6:10*3)
  expect_true(length(data_columns_as_list(d1, d2)) == 5)
})

test_that("keep_singles and keep_duplicate works", {
  original_model = model.matrix(~satisfaction*motivation + health, data=exercise_data)
  new_model = model.matrix(~motivation*satisfaction, data=exercise_data)
  expect_true(keep_singles(original_model, new_model)=="health")
  expect_false("health" %in% names(keep_duplicates(original_model, new_model)))

})

test_that("return_term_location works", {
  model = lm(weight.loss~therapy.type + motivation + health, data=exercise_data)
  expect_error(return_term_location(model, NULL))
  expect_error(return_term_location(model, "motiv"))
  expect_error(return_term_location(model, c("motiv", "health")))
  expect_equal(return_term_location(model, "therapy.type"), 2)
  expect_equal(return_term_location(model, c("therapy.type", "health")), c(2,4))
})

test_that("return_fitted_line works", {
  mod = lm(y~x+a, data=small)
  expect_equal(return_fitted_line(suppress_model=F), geom_blank())
  expect_equal(return_fitted_line(model=mod, suppress_model=T), geom_blank())
  return_fitted_line(p$data, plot_with_color, mod)
  return_fitted_line(p$data, plot_sans_color, mod)
})








