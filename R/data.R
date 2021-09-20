# Fake dataset ------------------------------------------------------------

#' Fake PO-SCORAD dataset
#'
#' The items are generated independently of each other.
#' - Extent is simulated with a "BinMC" model
#' - intensity signs are generated with an "OrderedRW" model
#' - subjective symptoms are generated with an "BinRW" model
#'
#' @format A named list with element `Data` and `Parameters` containing dataframes.
#' The dataframe `Data` contains the PO-SCORAD time-series.
#' It has the following columns:
#' \describe{
#'   \item{Patient}{Patient ID}
#'   \item{Day}{Day since first measurement (day=1 is the first measurement)}
#'   \item{Extent}{}
#'   \item{Itching VAS}{}
#'   \item{Sleep disturbance VAS}{}
#'   \item{Dryness}{}
#'   \item{Redness}{}
#'   \item{Swelling}{}
#'   \item{Scabs/Oozing}{}
#'   \item{Traces of scratching}{}
#'   \item{Thickening}{}
#'   \item{Intensity}{Dryness + Redness + Swelling + Scabs/Oozing + Traces of scratching + Thickening}
#'   \item{Subjective symptoms}{Itching VAS + Sleep disturbance VAS}
#'   \item{oSCORAD}{0.2 * Extent + 3.5 * Intensity}
#'   \item{SCORAD}{0.2 * Extent + 3.5 * Intensity + Subjective symptoms}
#' }
#' The dataframe `Parameters` contains the parameters' values of the model used to generate the data.
#' It has the following columns:
#' \describe{
#'   \item{Model}{Name of the model}
#'   \item{Item}{Name of the item}
#'   \item{Draw}{Draw used to generate the data}
#'   \item{Index}{Index of the parameter}
#'   \item{Value}{Parameter value}
#'   \item{Parameter}{Name of the parameter}
#' }
#'
"FakeData"

# Posterior estimates -----------------------------------------------------

#' Estimates of EczemaPred models
#'
#' The dataset contains the posterior and prior summary statistics of the main parameters
#' of the EczemaPred models used to predict PO-SCORAD.
#' The posterior are saved for the "Derexyl" (dataset 1) and "PFDC" dataset.
#'
#' @format A dataframe with columns
#' \describe{
#'   \item{Mean}{Mean}
#'   \item{se_mean}{Monte-Carlo standard-error}
#'   \item{sd}{Standard deviation}
#'   \item{5%}{5% quantile}
#'   \item{25%}{25% quantile}
#'   \item{50%}{50% quantile}
#'   \item{75%}{75% quantile}
#'   \item{95%}{95% quantile}
#'   \item{n_eff}{Effective sample size}
#'   \item{Rhat}{Rhat convergence diagnostic}
#'   \item{Variable}{Name of the variable/parameter}
#'   \item{Index}{(optional) index of the variable}
#'   \item{Distribution}{Either of "Prior", "Posterior - Derexyl" or "Posterior - PFDC"}
#'   \item{Model}{Name of the model}
#'   \item{Item}{Name of the severity item}
#' }
"par_POSCORAD"
