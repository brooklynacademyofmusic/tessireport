test_that("parse_shapley returns a human-redable version of a shapley analysis", {
  exp_filename <- rprojroot::find_testthat_root_file("test-iml_shapley.Rds")
  explanations <- readRDS(exp_filename)

  expect_snapshot(lapply(explanations,parse_shapley))

  # formats dates
  expect_match(sapply(explanations,parse_shapley) %>% grep(pattern="timestamp",value=T), "timestamp.+days")
  # and big numbers
  expect_match(sapply(explanations,parse_shapley) %>% grep(pattern="income",value=T), "\\d+,\\d+{3}")

})

test_that("iml_regularize returns a dataset with no missing values and no constant columns", {
  test <- data.table(x = seq(100))

  # no-op
  expect_equal(iml_regularize(copy(test)),test)

  # imputes by sampling
  test[1:10,x := NA]
  expect_equal(iml_regularize(copy(test))[11:100],test[11:100])
  expect_equal(iml_regularize(copy(test))[1:10,x] %in% test[11:100,x],rep(T,10))

  # adds noise
  test[1:99,x := NA]
  # noise added to first element
  expect_equal(iml_regularize(copy(test)) %>% {.[2:100,x] == .[1,x]}, rep(F,99))
  # equal within tolerance == sqrt(.Machine$double.eps) -- see [all.equal]
  expect_true(iml_regularize(copy(test))[,all.equal(x,rep(100,100))])

  # works when entire column is NA
  test[,x := NA]
  # noise added to first element
  expect_equal(iml_regularize(copy(test)) %>% {.[2:100,x] == .[1,x]}, rep(F,99))
  # equal within tolerance == sqrt(.Machine$double.eps) -- see [all.equal]
  expect_true(iml_regularize(copy(test))[,all.equal(x,rep(0,100))])

})
