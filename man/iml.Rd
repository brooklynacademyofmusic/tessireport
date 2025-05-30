% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iml.R
\name{iml}
\alias{iml}
\alias{iml_regularize}
\alias{iml_predictor}
\alias{iml_featureimp}
\alias{iml_featureeffects}
\alias{iml_shapley}
\title{Interpretable machine learning for mlr_report}
\usage{
iml_regularize(data, features = names(sapply(data, is.numeric)))

iml_predictor(
  model,
  data,
  predict.function = NULL,
  y = NULL,
  regularize = TRUE
)

iml_featureimp(
  model,
  data,
  loss = "logLoss",
  compare = "difference",
  n.repetitions = 5,
  features = NULL
)

iml_featureeffects(
  model,
  data,
  features = NULL,
  method = "ale",
  center.at = NULL,
  grid.size = 20
)

iml_shapley(model, data, x.interest = NULL, sample.size = 100)
}
\arguments{
\item{data}{\link[data.table:data.table]{data.table} of a test dataset}

\item{features}{\code{character} The names of the features for which to compute the feature effects/importance.}

\item{model}{\link[mlr3:Learner]{mlr3::Learner} model, pre-trained}

\item{predict.function}{\code{function} function to predict newdata. The first argument is the model, the second the newdata.}

\item{y}{\code{character(1)}|\link{numeric}|\link{factor} The target vector or (preferably) the name of the target column in the data argument. Predictor tries to infer the target automatically from the model.}

\item{regularize}{\code{logical(1)} whether or not to pass the data through \link{iml_regularize}}

\item{loss}{\code{character(1)}|\link{function}. The loss function. Either the name of a loss (e.g. "ce" for classification or "mse") or a function.}

\item{compare}{\code{character(1)} Either "ratio" or "difference".}

\item{n.repetitions}{\code{numeric(1)} How many shufflings of the features should be done?}

\item{method}{\code{character(1)}
\itemize{
\item 'ale' for accumulated local effects,
\item 'pdp' for partial dependence plot,
\item 'ice' for individual conditional expectation curves,
\item 'pdp+ice' for partial dependence plot and ice curves within the same plot.
}}

\item{center.at}{\code{numeric(1)} Value at which the plot should be centered. Ignored in the case of two features.}

\item{grid.size}{\code{numeric(1)} The size of the grid for evaluating the predictions.}

\item{x.interest}{\link{data.frame} data to be explained.}

\item{sample.size}{\code{numeric(1)} The number of Monte Carlo samples for estimating the Shapley value.}
}
\value{
\itemize{
\item \code{iml_predictor()}: \link[iml:Predictor]{iml::Predictor}
\item \code{iml_featureimp()}: \link[iml:FeatureImp]{iml::FeatureImp}
\item \code{iml_featureeffects()}: \link[iml:FeatureEffects]{iml::FeatureEffects}
\item \code{iml_shapley()}: \link[iml:Shapley]{iml::Shapley}
}
}
\description{
Simple wrappers around \link[iml:iml-package]{iml} classes to provide a more streamlined approach for generating
interpretable plots and explanatory data.
}
\section{Functions}{
\itemize{
\item \code{iml_regularize()}: regularize data for iml by replacing NA's and adding small
random noise to constant columns

\item \code{iml_predictor()}: wrapper for \link[iml:Predictor]{iml::Predictor} to subset the features of data and
provide a \code{predict.function} and \code{y} when the predictor can't identify
them.

\item \code{iml_featureimp()}: wrapper for \link[iml:FeatureImp]{iml::FeatureImp} that handles predictor creation and multiprocessing

\item \code{iml_featureeffects()}: wrapper for \link[iml:FeatureEffects]{iml::FeatureEffects} that handles data filtering, predictor creation and multiprocessing

\item \code{iml_shapley()}: wrapper for \link[iml:Shapley]{iml::Shapley} that handles predictor creation and multiprocessing

}}
