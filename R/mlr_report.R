#' @title mlr_report
#' @description Base class for training/running a machine learning model
#' @name mlr_report
#' @param mlr_report The report object
#' @param ... not used
#' @export
mlr_report <- report(class="mlr_report")

#' @export
#' @describeIn mlr_report Should load the dataset into `mlr_report$task`
#' and is in charge of labeling column roles (i.e. feature, target, group, weight)
read.mlr_report <- function(mlr_report, ...) {
  NextMethod()
}


#' @param train boolean whether to run the training
#' @param predict boolean whether to run the predictions
#' @export
#' @describeIn mlr_report Do the training and/or run the model. Subclasses
#' should define a `train.mlr_model` and `predict.mlr_model` function, which should load from
#' `mlr_report$task` and store their results in `mlr_report$model` and `mlr_report$predictions`
process.mlr_report <- function(mlr_report, train = TRUE, predict = TRUE, ...) {
  report_name <- class(mlr_report)[1]
  if (train) {
    mlr_report <- train(mlr_report, ...)
    save_model(model = mlr_report$model, model_name = "model.Rds", subdir = report_name, sync = FALSE)
  }

  if (predict) mlr_report <- predict(mlr_report, ...)
  NextMethod()
}

#' @describeIn mlr_report Train the model
#' @export
train <- function(...) UseMethod("train")
#' @export
train.mlr_report <- function(x, ...) x

#' @describeIn mlr_report Predict the model
#' @export
predict <- function(...) UseMethod("predict")
#' @export
predict.mlr_report <- function(x, ...) x


#' @export
#' @describeIn mlr_report Analyze the input set and the sensitivity of the training using
#' some standard heuristics.
output.mlr_report <- function(mlr_report, ...) {
  report_name <- class(mlr_report)[1]

  if (is.null(mlr_report$model))
    mlr_report$model <- load_model(report_name)

  if (is.null(mlr_report$dataset))
    mlr_report$dataset <- read(mlr_report)

  if (is.null(mlr_report$predictions))
    mlr_report$predictions <- read_cache("predictions",report_name)

  NextMethod()
}


#' @param subdir character name of subdirectory to store data in, defaults to the primary class of `mlr_report`
#' @param sync boolean whether to sync data across storages
#' @describeIn mlr_report Save the trained model and model output for future use. Writes the model to the
#' [tessilake::cache_primary_path] under the subdirectory `subdir` and then syncs them across storages.
#' @export
#' @importFrom tessilake write_cache sync_cache
write.mlr_report <- function(mlr_report, subdir = class(mlr_report)[1], sync = TRUE, ...) {

  if (!is.null(mlr_report$model))
    save_model(model = mlr_report$model, model_name = "model.Rds", subdir = subdir, sync = sync)

  if (!is.null(mlr_report$predictions))
    write_cache(mlr_report$predictions, "predictions", subdir, sync = sync, overwrite = TRUE)


  NextMethod()
}

#' @describeIn mlr_report load serialized mlr model from disk
#' @param subdir `character(1)` location where the model is stored
#' @param model_name `character(1)` filename of the model
load_model <- function(subdir = "model", model_name = "model.Rds") {
  path_name = cache_primary_path("", subdir)
  readRDS(file.path(path_name,model_name))
}

#' @describeIn mlr_report save serialized mlr model to disk
#' @importFrom tessilake cache_primary_path
#' @param model [mlr3::Learner] to be saved
save_model <- function(model, subdir = "model", model_name = "model.Rds", sync = TRUE) {
  path_name = cache_primary_path("", subdir)

  if(!dir.exists(path_name))
    dir.create(path_name, recursive = T)

  saveRDS(model, file.path(path_name,model_name))
  if (sync) sync_cache(model_name, subdir, whole_file = TRUE)
}


#' arrow_to_mlr3
#'
#' Converts arrow Table/Dataset to mlr3 Backend
#'
#' @param dataset [arrow::Table] or [arrow::Dataset]
#' @param primary_key character name of the column to use as a primary key
#'
#' @return [mlr3db::DataBackendDplyr]
#' @importFrom duckdb duckdb duckdb_register_arrow
#' @importFrom dplyr tbl
#' @importFrom mlr3db DataBackendDplyr
#' @importFrom checkmate assert_multi_class
#' @importFrom DBI dbConnect
arrow_to_mlr3 <- function(dataset, primary_key = "I") {
  assert_multi_class(dataset,c("Table","Dataset","arrow_dplyr_query"))
  assert_names(names(dataset), must.include = primary_key)

  filename <- file.path(tempdir(),"duckdb.parquet")
  arrow::write_parquet(dataset, filename)

  backend <- mlr3db::as_duckdb_backend(filename, primary_key = primary_key)
}

