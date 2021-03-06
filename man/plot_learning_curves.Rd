% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{plot_learning_curves}
\alias{plot_learning_curves}
\title{Plot learning curves}
\usage{
plot_learning_curves(perf, metric = NULL, fc_it = NULL)
}
\arguments{
\item{perf}{Dataframe of performance estimate}

\item{metric}{(optional) name of the metric to plot to set the scale and label.
When \code{metric = "lpd"}, the x-axis is labelled in the context of a discrete forecast.}

\item{fc_it}{(optional) Dataframe output from \code{detail_fw_training} to make a secondary x axis}
}
\value{
Ggplot
}
\description{
Plot learning curves
}
