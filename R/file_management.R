# File management ---------------------------------------------------------

#' @rdname get_results_files
#' @noRd
get_prior_files <- function(outcome, model) {

  suff <- paste0(outcome, "_", model, ".rds")
  out <- list(
    PriorFit = file.path("results", paste0("prior_", suff)),
    PriorPar = file.path("results", paste0("par0_", suff)),
    FakeFit = file.path("results", paste0("fake_", suff))
  )
  return(out)
}

#' @rdname get_results_files
#' @noRd
get_fit_files <- function(outcome, model, dataset) {

  suff <- paste0(outcome, "_", model, "_", dataset, ".rds")
  out <- list(
    Fit = file.path("results", paste0("fit_", suff)),
    FitPar = file.path("results", paste0("par_", suff))
  )
  return(out)
}

#' @rdname get_results_files
#' @noRd
get_validation_files <- function(outcome, model, dataset, val_horizon) {

  suff <- paste0(outcome, "_", model, "_", dataset)
  out <- list(
    ValDir = file.path("results", paste0("val", val_horizon, "_",  suff)),
    Val = file.path("results", paste0("val", val_horizon, "_",  suff, ".rds"))
  )
  return(out)
}

#' Dictionary of results files
#'
#' @param outcome item/score to predict
#' @param model model name
#' @param dataset dataset name
#' @param val_horizon validation horizon
#'
#' @return Named list:
#' - Prior: Stanfit object of prior predictive distribution
#' - PriorPar: Dataframe of posterior summary statistics of parameters
#' - FakeFit: Stanfit object of fake data fit
#' In addition, if `dataset` is not NULL:
#' - Fit: Stanfit object of the fit
#' - FitPar: Dataframe of posterior summary statistics of parameters
#' In addition, if `val_horizon` is not NULL:
#' - ValDir: Temporary directory storing individual iteration results
#' - Val: Full validation results
#'
#' @export
#' @import rlang HuraultMisc
get_results_files <- function(outcome, model, dataset = NULL, val_horizon = NULL) {

  stopifnot(is_scalar_character(outcome),
            is_scalar_character(model))

  intensity_signs <- detail_POSCORAD("Intensity signs")$Name

  if (outcome %in% intensity_signs) {
    out <- get_prior_files("intensity", model)
  } else if (outcome %in% c("itching", "sleep")) {
    out <- get_prior_files("subjective", model)
  } else {
    out <- get_prior_files(outcome, model)
  }

  if (!is.null(dataset)) {
    stopifnot(is_scalar_character(dataset))
    out <- c(out, get_fit_files(outcome, model, dataset))
    if (!is.null(val_horizon)) {
      stopifnot(is_scalar_wholenumber(val_horizon))
      out <- c(out, get_validation_files(outcome, model, dataset, val_horizon))
    }
  }

  return(out)
}
