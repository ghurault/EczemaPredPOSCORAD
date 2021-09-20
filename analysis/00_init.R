
library(EczemaPredPOSCORAD)
library(EczemaPred)
library(HuraultMisc)
# library(TanakaData)
library(tidyverse)
library(cowplot)
library(here)

library(rstan)
# rstan_options(auto_write = TRUE) # Save compiled model
options(mc.cores = parallel::detectCores()) # Parallel computing

# Data processing ---------------------------------------------------------

#' Load Derexyl or PFDC dataset containing POSCORAD
#'
#' - Remove patients with less than x=5 observations
#' - Regenerate patient ID (BEWARE for comparisons with other datasets!)
#'
#' This function requires the proprietary package `TanakaData`, except when `dataset = "Fake"`.
#'
#' @param dataset Name of the dataset
#'
#' @return
#'
#' @import dplyr
load_dataset <- function(dataset = c("Derexyl", "PFDC", "Fake")) {

  dataset <- match.arg(dataset)

  if (dataset == "Derexyl") {
    out <- TanakaData::POSCORAD_Derexyl
  }
  if (dataset == "PFDC") {
    out <- TanakaData::POSCORAD_PFDC
  }
  if (dataset == "Fake") {
    out <- EczemaPredPOSCORAD::FakeData$Data
  }

  out <- out %>%
    group_by(Patient) %>%
    filter(n() >= 5) %>%
    mutate(Patient = cur_group_id()) %>%
    ungroup()

  return(out)
}
