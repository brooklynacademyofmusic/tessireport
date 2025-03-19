withr::local_package("checkmate")
withr::local_package("mockery")

# contributions_dataset ------------------------------------------------

test_that("contributions_dataset reads from cache and adds an event indicator", {
  tessilake::local_cache_dirs()

  read_cache <- mock(arrow::arrow_table(
    group_customer_no = 1,
    event_type = c("Ticket","Contribution"),
    contribution_amt = 50,
    timestamp = Sys.Date()+c(-60,-.001),
    rowid = 1:2),
    arrow::arrow_table(date = lubridate::POSIXct())
  )

  stub(contributions_dataset, "read_cache", read_cache)

  contributions_dataset()

  rm(read_cache)
  dataset <- read_cache("dataset","contributions_model") %>% collect
  expect_equal(nrow(dataset),2)
  expect_equal(dataset[,"event"][[1]],c(F,T))

})

test_that("contributions_dataset rebuilds all data when rebuild_dataset = TRUE", {
  tessilake::local_cache_dirs()

  read_cache <- mock(arrow::arrow_table(
    rowid = 1:2,
    group_customer_no = 1,
    event_type = c("Ticket","Contribution"),
    contribution_amt = 50,
    timestamp = Sys.Date()+c(-60,-.001)),
    arrow::arrow_table(date = lubridate::POSIXct())
  )

  stub(contributions_dataset, "read_cache", read_cache)
  contributions_dataset()

  stub(contributions_dataset, "read_cache", mock(arrow::arrow_table(
      rowid = 1:4,
      group_customer_no = 1:4,
      event_type = rep(c("Ticket","Contribution"),2),
      contribution_amt = 50,
      timestamp = Sys.Date()+c(-60,-.001,365,366)),
      arrow::arrow_table(date = lubridate::POSIXct())))

  dataset_chunk_write <- mock(T)
  stub(contributions_dataset, "dataset_chunk_write", dataset_chunk_write)
  debugonce(contributions_dataset)
  contributions_dataset(rebuild_dataset = T, chunk_size = 2)

  expect_length(mock_args(dataset_chunk_write),1)
  expect_equal(mock_args(dataset_chunk_write)[[1]][["partition"]], 1)

})

test_that("contributions_dataset only appends data when rebuild_dataset = NULL", {
  tessilake::local_cache_dirs()

  stub(contributions_dataset, "read_cache", mock(arrow::arrow_table(
    rowid = 1:2,
    group_customer_no = 1,
    event_type = c("Ticket","Contribution"),
    contribution_amt = 50,
    timestamp = Sys.Date()+c(-60,-.001)),
    arrow::arrow_table(date = lubridate::POSIXct())
  ))

  contributions_dataset()

  stub(contributions_dataset, "read_cache", mock(
    read_cache("dataset","contributions_model"),
    arrow::arrow_table(
      rowid = 1:4,
      group_customer_no = 1:4,
      event_type = rep(c("Ticket","Contribution"),2),
      contribution_amt = 50,
      timestamp = Sys.Date()+c(-60,-.001,365,366)),
      arrow::arrow_table(date = lubridate::POSIXct()), cycle = T))

  dataset_chunk_write <- mock(T,cycle = T)
  stub(contributions_dataset, "dataset_chunk_write", dataset_chunk_write)

  contributions_dataset()
  expect_length(mock_args(dataset_chunk_write),0)

  contributions_dataset(until = Inf, chunk_size = 2)
  expect_length(mock_args(dataset_chunk_write),1)
  expect_equal(mock_args(dataset_chunk_write)[[1]][["partition"]], 2)

})

test_that("contributions_dataset only reads data when rebuild_dataset = FALSE", {
  tessilake::local_cache_dirs()

  stub(contributions_dataset, "read_cache", mock(arrow::arrow_table(
    rowid = 1:2,
    group_customer_no = 1,
    event_type = c("Ticket","Contribution"),
    contribution_amt = 50,
    timestamp = Sys.Date()+c(-60,-.001)),
    arrow::arrow_table(date = lubridate::POSIXct()),
    read_cache("dataset","contributions_model"),
    cycle = T
  ))

  contributions_dataset()

  dataset_chunk_write <- mock(T)
  stub(contributions_dataset, "dataset_chunk_write", dataset_chunk_write)

  contributions_dataset(rebuild_dataset = F)

  expect_length(mock_args(dataset_chunk_write),0)

})

# read.contributions_model ------------------------------------------------

test_that("read.contributions_model creates a valid mlr3 classification task", {
  stub(read.contributions_model, "contributions_dataset",
       \(...) {arrow::read_parquet(rprojroot::find_testthat_root_file("test-contributions_model.parquet"), as_data_frame = F)})

  stub(read.contributions_model,"cache_exists_any",TRUE)

  model <<- read(contributions_model, predict_since = as.Date("2024-06-01"),
                 downsample_read = 1, rebuild_dataset = F)

  expect_class(model$task, "TaskClassif")

})

test_that("read.contributions_model creates a valid mlr3 validation task", {

  expect_class(model$task$internal_valid_task, "TaskClassif")
  data <- model$task$internal_valid_task$data(cols = "date")
  expect_true(all(data$date >= as.Date("2024-06-01")))

})

if(Sys.getenv("MODEL_TESTS")=="TRUE") {
# train.contributions_model -----------------------------------------------
tessilake::local_cache_dirs()
test_that("train.contributions_model successfully trains a model", {
  dataset <- arrow::read_parquet(rprojroot::find_testthat_root_file("test-contributions_model.parquet"), as_data_frame = F)
  stub(read.contributions_model, "contributions_dataset",\(...) {dataset})

  stub(read.contributions_model,"cache_exists_any",TRUE)

  dates <- dataset %>% select(date) %>% collect

  .model <- read(contributions_model, predict_since = median(dates$date),
                 downsample_read = 1, rebuild_dataset = F)

  future::plan("sequential")

  suppressWarnings(model <<- train(.model, num_trees = 16, downsample_train = 1))

  expect_class(model$model, "Learner")

})

# predict.contributions_model ---------------------------------------------

test_that("predict.contributions_model successfully predicts new data", {

  model <<- predict(model)

  expect_data_table(model$predictions)
  expect_names(names(model$predictions),must.include = c("group_customer_no","date","truth","prob.TRUE"))

})

# output.contributions_model ---------------------------------------------

test_that("output.contributions_model successfully interprets the model", {
  dataset <- \(...) {arrow::read_parquet(rprojroot::find_testthat_root_file("test-contributions_model.parquet"), as_data_frame = F)}
  stub(output, "contributions_dataset",dataset,2)
  stub(read.contributions_model, "contributions_dataset",dataset,2)

  withr::local_options(future.globals.maxSize = 1024^3)

  dir.create(cache_primary_path("","contributions_model"))

  read(model)
  output(model, features = c("contribution_amt","contribution_timestamp_max"), n.repetitions = 1, n_features = 5, n_top = 5)

  pdf_filename <- cache_primary_path("contributions_model.pdf","contributions_model")
  exp_filename <- cache_primary_path("shapley.Rds","contributions_model")
  expect_file_exists(pdf_filename)
  expect_file_exists(exp_filename)

  explanations <- readRDS(exp_filename)
  expect_data_table(explanations)
  expect_names(colnames(explanations), must.include = "explanation")

})
}


