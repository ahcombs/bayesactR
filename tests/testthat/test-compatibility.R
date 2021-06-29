test_that("dict-type compatibility", {
  expect_true(check_dict_type(c("usfullsurveyor2015", "uga2015bayesactsubset", "nc1978", "uga2015bayesactsubset"), c("mean", "sd", "mean", "cov")))
  expect_error(check_dict_type(c("usfullsurveyor2015", "uga2015bayesactsubset", "nc1978", "uga2015bayesactsubset"), c("sd", "sd", "mean", "cov")))
  expect_error(check_dict_type(c("usfullsurveyor2015", "uga2015bayesactsubset", "nc1978", "uga2015bayesactsubset"), c("mean", "mean", "mean", "cov")))
})

test_that("dict-gender compatibility", {
  expect_true(check_dict_gender(c("usfullsurveyor2015", "nc1978", "nc1978", "politics2003"), c("av", "male", "female", "av")))
  expect_error(check_dict_gender(c("usfullsurveyor2015", "nc1978", "usfullsurveyor2015", "politics2003"), c("female", "male", "female", "av")))
})

test_that("dict-component compatibility", {
  expect_true(check_dict_components(c("usfullsurveyor2015", "nc1978", "usfullsurveyor2015", "nc1978")))
  expect_error(check_dict_components(c("usfullsurveyor2015", "nc1978", "usfullsurveyor2015", "politics2003")))
})

test_that("equation-gender compatibility", {
  expect_true(check_eqn_gender(c("us2010", "nc1978"), c("av", "male")))
  expect_error(check_eqn_gender(c("us2010", "nc1978"), c("female", "male")))
})

test_that("probability sums", {
  expect_true(check_probs(agent_ident_prob = c("1"), object_ident_prob = c("1")))
  expect_true(check_probs(agent_ident_prob = c(".5", ".5"), object_ident_prob = c(".5", ".5")))
  expect_true(check_probs(agent_ident_prob = c(".99", ".01"), object_ident_prob = c(".01", ".99")))
  expect_error(check_probs(agent_ident_prob = c("1", ".01"), object_ident_prob = c(".01", ".99")))
})
