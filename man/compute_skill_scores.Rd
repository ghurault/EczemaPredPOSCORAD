% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{compute_skill_scores}
\alias{compute_skill_scores}
\title{Compute skill scores}
\usage{
compute_skill_scores(res, ref_mdl, metrics)
}
\arguments{
\item{res}{Dataframe of predictions (with different models)}

\item{ref_mdl}{Name of the reference model}

\item{metrics}{Names of metrics to normalise by the reference model}
}
\value{
Dataframe res with additional columns corresponding to the skill scores
(name metric + suffix "_diff" or (C)RPSS if metric is (C)RPS)
}
\description{
By default, the skill score is just the metric minus the metric of the reference model.
If \code{metric} is (C)RPS or CRPS, the (C)RPSS is computed.
}
