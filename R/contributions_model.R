#' @title contributions_model
#' @description mlr3 model for predicting a customer's first contribution
#' @export
#' @name contributions_model
contributions_model <- report(class=c("contributions_model","mlr_report"))

#' @describeIn contributions_model Build the contributions dataset from the overall stream.
#' * events are the first contribution per household > $50
#' * data after an event are censored
#' * contribution indicators are rolled back and timestamps are normalized to the start of customer activity
#' * only data since `since` are loaded
#' Data is written to the primary cache partitioned by year and then synced across storages
#' @param since Date/POSIXct data on or after this date will be loaded and possibly used for training
#' @param chunk_size integer(1) number of rows per partition
#' @param ... not used
#' @importFrom tidyselect all_of
#'
contributions_dataset <- function(since = Sys.Date()-365*5, until = Sys.Date(),
                                  rebuild_dataset = NULL, chunk_size = 1e7, ...) {


  . <- stream <- group_customer_no <- timestamp <- event_type <- event <- contribution_amt <- n_event <-
    N <- partition <- NULL

  dataset_max_date <- NULL
  if (is.null(rebuild_dataset) && cache_exists_any("dataset","contributions_model") || !(rebuild_dataset %||% T)) {
    dataset <- read_cache("dataset","contributions_model")
    dataset_max_date <- summarise(dataset,max(date,na.rm = T)) %>% collect %>% .[[1]]

    if(dataset_max_date >= until || !(rebuild_dataset %||% T))
      return(dataset %>% filter(date >= since & date < until))
  }

  stream <- read_cache("stream", "stream", include_partition = TRUE)
  assert_names(names(stream),must.include = c("group_customer_no","timestamp","event_type","contribution_amt","rowid"))

  stream_key <- stream %>%
    select(all_of(c("group_customer_no","timestamp","event_type","contribution_amt","rowid"))) %>%
    collect %>% setDT

  setkey(stream_key,group_customer_no,timestamp)

  # add event indicator
  stream_key[,event := event_type == "Contribution" & contribution_amt>=50]
  stream_key[,`:=`(n_event = cumsum(event),
                   N = .N), by="group_customer_no"]
  # censor
  stream_key <- stream_key[n_event == 0 | event & n_event==1 & N>1]
  stream_key[,`:=`(n_event = NULL, N = NULL)]

  # filter dates
  stream_key <- stream_key[timestamp >= dataset_max_date %||% since & timestamp < until]

  # subsample
  stream_key[, `:=`(date = as.Date(timestamp),
  month = lubridate::floor_date(timestamp, "months"))]
  setorder(stream_key, group_customer_no, month, event, timestamp)
  stream_key <- stream_key[, last(.SD), by = c("group_customer_no", "month")]
  stream_key$month <- NULL

  # partition by fixed number of rows
  stream_key[,partition := ceiling(rowid/chunk_size)]

  if(nrow(stream_key) > 0) {
    stream_key[,dataset_chunk_write(dataset = stream, partition = partition,
                                    dataset_name = "contributions_model",
                                    rows = .SD,
                                    cols = grep("Adj",colnames(stream),value=T,invert = T),
                                    timestamp_cols = setdiff(grep("timestamp",colnames(stream),value=T,ignore.case = T),"timestamp_id"),
                                    rollback_cols = grep("^(email|contribution|ticket)",colnames(stream),value = T) %>%
                                      grep(pattern = "Adj",value=T,invert = T)),
               by = "partition"]
    tessilake::sync_cache("dataset", "contributions_model", overwrite = TRUE, partition = "partition")
  }


  return(read_cache("dataset","contributions_model") %>%
           filter(date >= since & date < until))

}

#' @export
#' @importFrom tessilake read_cache cache_exists_any
#' @importFrom dplyr filter select collect mutate summarise
#' @importFrom mlr3 TaskClassif
#' @describeIn contributions_model Read in contribution data and prepare a mlr3 training task and a prediction/validation task
#'
#' @param model `contributions_model` object
#' @param predict_since Date/POSIXct data on/after this date will be used to make predictions and not for training
#' @param predict Not used, just here to prevent partial argument matching
#' @param until Date/POSIXct data after this date will not be used for training or predictions, defaults to the beginning of today
#' @param rebuild_dataset boolean rebuild the dataset by calling `contributions_dataset(since=since,until=until)` (TRUE), just read the existing one (FALSE),
#' or append new rows by calling `contributions_dataset(since=max_existing_date,until=until)` (NULL, default)
#' @param downsample_read `numeric(1)` the amount to downsample the dataset on read
#' @note Data will be loaded in-memory, because *\[inaudible\]* mlr3 doesn't work well with factors encoded as dictionaries in arrow tables.
read.contributions_model <- function(model,
                                     since = Sys.Date()-365*5,
                                     until = Sys.Date(),
                                     predict_since = Sys.Date() - 30,
                                     rebuild_dataset = NULL,
                                     downsample_read = 1,
                                     predict = NULL, ...) {

  . <- event <- date <- TRUE

  dataset <- contributions_dataset(since = since, until = until,
                                   rebuild_dataset = rebuild_dataset)

  dataset <- rbind(filter(dataset, date >= predict_since | event == "TRUE") %>%
                     collect %>% setDT,
                   filter(dataset, date < predict_since & event == "FALSE") %>%
                     dplyr::slice_sample(prop = downsample_read) %>%
                     collect %>% setDT) %>%
    .[,`:=`(event = factor(event, levels = c("FALSE","TRUE")),
            date = as.POSIXct(date))]


  # convert numeric and date to numeric
  numeric_cols <- names(dataset)[which(sapply(dataset, \(.) is.numeric(.) | is.Date(.)))] %>% setdiff("event")
  dataset[,(numeric_cols) := lapply(.SD,as.numeric), .SDcols = numeric_cols]
  # eliminate fully missing columns (for iml)
  non_missing_cols <- names(dataset)[which(sapply(dataset, \(.) any(!is.na(.))))]
  dataset <- dataset[,non_missing_cols,with=F]

  model$dataset <- dataset
  model$task <- TaskClassif$new(id = "contributions",
                                target = "event",
                                backend = dataset)

  # label columns + rows
  model$task$col_roles$feature <- grep("^(email|contribution|address|ticket).+(amt|count|level|max|min)|^timestamp$",
                                       names(dataset),value=T,ignore.case=T,perl=T)

  model$task$col_roles$order <- "timestamp"
  model$task$col_roles$group <- "group_customer_no"
  model$task$positive <- "TRUE"

  model$task$internal_valid_task <- dataset[date >= predict_since,which=T]

  NextMethod()
}

#' @export
#' @importFrom tessilake cache_primary_path
#' @importFrom mlr3verse po to_tune flt lts ppl tune `%>>%` p_int tnr selector_invert selector_grep
#' @importFrom mlr3 msr rsmp as_learner lrn
#' @importFrom bbotk mlr_optimizers
#' @importFrom bestNormalize yeojohnson
#' @importFrom ranger ranger
#' @describeIn contributions_model Tune and train a stacked log-reg/ranger model on the data
#' @details
#' # Preprocessing:
#' * ignore 1-day and email "send" features because they leak data
#' * remove constant features
#' * balance classes to a ratio of 1:10 T:F
#' * Yeo-Johnson with tuned boundaries
#' * impute missing values out-of-range and add missing data indicator features
#' * feature importance filter (for log-reg submodel only)
#' # Model:
#' * stacked log-reg + ranger > log-reg model
#' * tuned using a hyperband method on the AUC (sensitivity/specificity)
#' @param num_trees `integer(1)` maximum number of trees to use for ranger model
#' @param downsample_train `double(1)` fraction of observations to use for training, defaults to .1
train.contributions_model <- function(model, num_trees = 512, downsample_train = .1, ...) {

  preprocess <- po("select",selector = selector_invert(selector_grep("__1|Send", perl = T, ignore.case = T))) %>>%
                po("classbalancing", reference = "minor",ratio = 1/downsample_train,adjust="downsample") %>>%
                ppl("robustify") %>>%
                po("filter", filter = flt("find_correlation"), filter.cutoff = .5) %>>%
                po("yeojohnson", lower = to_tune(-2,0), upper = to_tune(0,2), eps = .1)

  importance_filter <- po("filter",
                   filter = flt("importance"),
                   filter.frac = to_tune(.1,.67))

  logreg <- as_learner(importance_filter %>>% lrn("classif.log_reg", predict_type = "prob"))
  ranger <- as_learner(lrn("classif.ranger", predict_type = "prob",
                           mtry.ratio = to_tune(.1,1),
                           sample.fraction = to_tune(.1,1),
                           num.trees = to_tune(p_int(num_trees/8,num_trees,tags="budget"))))

  stacked <- as_learner(preprocess %>>% ppl("stacking", c(logreg,ranger),
                                                           lrn("classif.log_reg", predict_type = "prob"),
                                                           use_features = FALSE))
  stacked_tuned <- tune(
      tuner = tnr("hyperband",eta=2),
      task = model$task,
      learner = stacked,
      resampling = rsmp("holdout"),
      measures = msr("classif.auc"))

  stacked$param_set$values <- stacked_tuned$result_learner_param_vals

  model$model <- stacked$train(model$task)

  NextMethod()
}

#' @export
#' @importFrom tessilake cache_primary_path read_cache
#' @importFrom dplyr filter
#' @describeIn contributions_model Predict using the trained model
predict.contributions_model <- function(model, ...) {
  if(is.null(model$model))
    model$model <- load_model("contributions_model")

  model$predictions <-
    cbind(as.data.table(model$model$predict(model$task$internal_valid_task)),
          model$task$internal_valid_task$data(cols = c("rowid","group_customer_no","date")))

  NextMethod()
}

#' @describeIn contributions_model create IML reports for contributions_model
#' @importFrom dplyr inner_join
#' @importFrom ggplot2 coord_flip theme_minimal theme scale_y_discrete element_text element_blank
#' @importFrom purrr walk
#' @importFrom tessilake cache_primary_path
#' @importFrom stats runif
#' @param n_top `integer(1)` the number of rows, ranked by probability to analyze and explain as 'top picks'.
#' @param n_features `integer(1)` the number of features, ranked by importance to analyze.
#' @param n_repetitions `numeric(1)` How many shufflings of the features should be done? See [iml::FeatureImp] for more info.
#' @param downsample_output `numeric(1)` the proportion of the test set to use for feature importance and
#' Shapley explanations
#' @inheritParams iml_featureimp
#' @export
output.contributions_model <- function(model, downsample_output = 1,
                                       features = NULL, n_repetitions = 5,
                                       n_top = 500, n_features = 25, ...) {
  prob.TRUE <- explanation <- NULL

  model <- NextMethod()

  model$dataset <- mutate(model$dataset, date = as.Date(date)) %>% collect %>% setDT
  model$predictions <- mutate(model$predictions, date = as.Date(date)) %>% collect %>% setDT

  dataset_predictions <- merge(model$dataset,
                                    model$predictions,
                                    by = c("group_customer_no","date","rowid"))
  setorder(dataset_predictions,-prob.TRUE)

  # downsample
  downsampled <- dataset_predictions[runif(.N)<downsample_output]

  # select top rows and fill with small random data
  top_picks <- dataset_predictions[seq(n_top)]

  # Feature importance
  fi <- iml_featureimp(model$model, downsampled, features = features, n.repetitions = n_repetitions)
  top_features <- na.omit(fi$results[seq(n_features),"feature"])

  pfi <- plot(fi) + coord_flip() +
    theme_minimal(base_size = 8) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_discrete(limits=rev)
  # remove styling of points
  walk(pfi$layers, \(.) .$aes_params <- list())

  # Feature effects across whole population
  fe1 <- iml_featureeffects(model$model, downsampled, top_features)
  pfe1 <- plot(fe1, fixed_y = F) &
    theme_minimal(base_size = 8) + theme(axis.title.y = element_blank())

  # Feature effects for top picks
  fe2 <- iml_featureeffects(model$model, top_picks, top_features)
  pfe2 <- plot(fe2, fixed_y = F) &
    theme_minimal(base_size = 8) + theme(axis.title.y = element_blank())

  pdf_filename <- cache_primary_path("contributions_model.pdf","contributions_model")
  write_pdf({
    pdf_plot(pfi, "Global feature importance","First contributions model")
    pdf_plot(pfe1, "Local feature effects","First contributions model, full dataset")
    pdf_plot(pfe2, "Local feature effects","First contributions model, top picks")
  }, .title = "Contributions model", output_file = pdf_filename)

  # Shapley explanations
  ex <- iml_shapley(model$model, downsampled,
                    x.interest = top_picks, sample.size = n_repetitions)

  top_picks[,explanation := map(ex,"results")]
  saveRDS(top_picks, cache_primary_path("shapley.Rds", "contributions_model"))

  NextMethod()

}

