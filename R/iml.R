#' @name iml
#' @title Interpretable machine learning for mlr_report
#' @description
#' Simple wrappers around [iml][iml::iml-package] classes to provide a more streamlined approach for generating
#' interpretable plots and explanatory data.
#' @return
#' * `iml_predictor()`: [iml::Predictor]
#' * `iml_featureimp()`: [iml::FeatureImp]
#' * `iml_featureeffects()`: [iml::FeatureEffects]
#' * `iml_shapley()`: [iml::Shapley]
#' @importFrom iml FeatureImp FeatureEffects Shapley
NULL

#' @importFrom iml Predictor
#' @importFrom purrr reduce
#'
#' @describeIn iml wrapper for [iml::Predictor] to subset the features of data and
#' provide a `predict.function` and `y` when the predictor can't identify
#' them.
#'
#' @param model [mlr3::Learner] model, pre-trained
#' @param data [data.table][data.table::data.table-package] of a test dataset
#' @param predict.function `function` function to predict newdata. The first argument is the model, the second the newdata.
#' @param y `character(1)`|[numeric]|[factor] The target vector or (preferably) the name of the target column in the data argument. Predictor tries to infer the target automatically from the model.
#'
iml_predictor <- function(model, data, predict.function = NULL, y = NULL) {

  # patch for prob models
  if (model$predict_type == "prob") {
    predict.function <- function(model,newdata) {
      model$predict_newdata(newdata)$prob[,"TRUE"]
    }
    y <- "prob.TRUE"
  } else {
    y <- "response"
  }

  # features used in the model
  features <- reduce(model$model,\(f,m) c(f,m$intasklayout$id)) %>% unlist %>% unique %>%
  # plus the output variable, filtering out non-existent columns
    c(y) %>% intersect(colnames(data))

  Predictor$new(model, data[,features,with=F],
                predict.function = predict.function,
                y = y)

}

#' @describeIn iml wrapper for [iml::FeatureImp] that handles predictor creation and multiprocessing
#' @param loss `character(1)`|[function]. The loss function. Either the name of a loss (e.g. "ce" for classification or "mse") or a function.
#' @param compare `character(1)` Either "ratio" or "difference".
#' @param n.repetitions `numeric(1)` How many shufflings of the features should be done?
iml_featureimp <- function(model, data, loss = "logLoss", compare = "difference",
                           n.repetitions = 5, features = NULL) {
  future::plan("multisession")
  predictor <- iml_predictor(model, data)
  fi <- FeatureImp$new(predictor, loss = loss, compare = compare,
                       n.repetitions = n.repetitions, features = features)
}

#' @describeIn iml wrapper for [iml::FeatureEffects] that handles data filtering, predictor creation and multiprocessing
#' @param features `character` The names of the features for which to compute the feature effects/importance.
#' @param method `character(1)`
#' * 'ale' for accumulated local effects,
#' * 'pdp' for partial dependence plot,
#' * 'ice' for individual conditional expectation curves,
#' * 'pdp+ice' for partial dependence plot and ice curves within the same plot.
#' @param center.at `numeric(1)` Value at which the plot should be centered. Ignored in the case of two features.
#' @param grid.size `numeric(1)` The size of the grid for evaluating the predictions.
iml_featureeffects <- function(model, data, features = NULL, method = "ale",
                               center.at = NULL, grid.size = 20) {
  future::plan("sequential")

  # replace missing data with OOB
  for (col in names(which(sapply(data, \(.) is.numeric(.) & !all(is.na(.))))))
    setnafill(data, "const", min(data[,col,with=F],na.rm=T)-
                sd(data[,col,with=F],na.rm=T)-
                .Machine$double.eps, cols = col)

  predictor <- iml_predictor(model, data)

  fe <- FeatureEffects$new(predictor, features = features, method = method,
                           center.at = center.at, grid.size = grid.size)
}

#' @describeIn iml wrapper for [iml::Shapley] that handles predictor creation and multiprocessing
#' @param x.interest [data.frame] data to be explained.
#' @param sample.size `numeric(1)` The number of Monte Carlo samples for estimating the Shapley value.
#' @importFrom furrr future_map
iml_shapley <- function(model, data, x.interest = NULL, sample.size = 100) {
  . <- NULL
  future::plan("multisession")
  predictor <- iml_predictor(model, data)

  s <- Shapley$new(predictor, x.interest = x.interest[1,], sample.size = sample.size)
  explanations <- x.interest[,predictor$data$feature.names,with=F] %>%
    split(seq_len(nrow(.))) %>%
    future_map(\(.) {s$explain(as.data.frame(.)); s$clone()})
}

#' parse_shapley
#'
#' Parse the data returned from [iml::Shapley] into a single single. Explanations are sorted by importance, filtered
#' by `filter`, the top `n` are collected and formatted into a single string.
#'
#' @param explanation [data.frame] of explanations as returned by [iml::Shapley]
#' @param filter `character(1)` regular expression of feature/value combinations to exclude
#' @param n `integer(1)` maximum number of explanatory features to return
#' @importFrom checkmate assert_data_frame assert_names
#' @importFrom utils head
#' @return string explaining the feature values
parse_shapley <- function(explanation, filter = "=0|=NA", n = 3) {

  . <- rank <- phi <- phi.var <- feature.value <- value <- value_fmt <- feature <- NULL

  assert_data_frame(explanation)
  assert_names(colnames(explanation), must.include = c("phi","phi.var","feature.value"))

  setDT(explanation)
  explanation[, rank := phi - 1.98*phi.var]
  setorder(explanation, -rank)
  features <- explanation[!grepl(filter,feature.value,perl=T)] %>% head(n = n) %>%
    .[,value := as.numeric(stringr::str_split_i(feature.value,"=",2))] %>%
    .[grepl("timestamp",feature,ignore.case=T), value_fmt := format(as.difftime(round(value/86400), units = "days"),
                                                                    trim = T, big.mark = ",")] %>%
    .[!grepl("timestamp",feature,ignore.case=T), value_fmt := format(value, trim = T, big.mark = ",")] %>%
    .[,paste0(feature, ": ", value_fmt, collapse = "; ")]

}
