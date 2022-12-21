test_that("dict-stat compatibility", {
  expect_true(check_dict_stat(c("usfullsurveyor2015", "uga2015", "nc1978", "uga2015"), c("mean", "sd", "mean", "cov")))
  expect_error(check_dict_stat(c("germany2007", "uga2015", "nc1978", "uga2015"), c("sd", "sd", "mean", "cov")))
  expect_error(check_dict_stat(c("usfullsurveyor2015", "uga2015", "nc1978", "germany2007"), c("mean", "mean", "mean", "cov")))
})

test_that("dict-group compatibility", {
  expect_true(check_dict_group(c("usfullsurveyor2015", "nc1978", "nc1978", "politics2003"), c("all", "male", "female", "all")))
  expect_error(check_dict_group(c("usfullsurveyor2015", "nc1978", "usfullsurveyor2015", "politics2003"), c("female", "male", "female", "all")))
})

test_that("dict-component compatibility", {
  expect_true(check_dict_components(c("usfullsurveyor2015", "nc1978", "usfullsurveyor2015", "nc1978")))
  expect_error(check_dict_components(c("usfullsurveyor2015", "nc1978", "usfullsurveyor2015", "politics2003")))
})

test_that("probability sums", {
  expect_true(check_probs(agent_ident_prob = c("1"), object_ident_prob = c("1")))
  expect_true(check_probs(agent_ident_prob = c(".5", ".5"), object_ident_prob = c(".5", ".5")))
  expect_true(check_probs(agent_ident_prob = c(".99", ".01"), object_ident_prob = c(".01", ".99")))
  expect_error(check_probs(agent_ident_prob = c("1", ".01"), object_ident_prob = c(".01", ".99")))
})

test_that("optional agent arg check", {
  expect_true(check_agent_opt_args(opt_args <- data.frame(alphas = c("1"))))
  expect_warning(check_agent_opt_args(opt_args <- data.frame(alpha = c("1"))))
  expect_error(check_agent_opt_args(opt_args <- data.frame(alphas = c("-1"))))
  expect_true(check_agent_opt_args(opt_args <- data.frame()))
  expect_true(check_agent_opt_args(opt_args <- ""))
})

