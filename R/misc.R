# Data utilities ----------------------------------------------------------
# cf. function from TanakaData package, imported as it is required by other functions

#' List and details available severity score and items in the Derexyl and PFDC dataset
#'
#' @param filter_output Optional character vector to subset the output by the "Name" column.
#' It can also take the value "Components" to select A (extent), B and C;
#' "Scores" to select SCORAD and oSCORAD;
#' "Items" to select all severity items;
#' "Intensity signs" to select all intensity signs;
#' and "Subjective symptoms" to select all subjective symptoms.
#' If `length(filter_output) > 1`, it acts as a OR.
#'
#' @return Dataframe with columns:
#' - Name: short name of the severity item/score
#' - Label: full name of the severity item/score in the dataset
#' - Maximum: maximum value that the severity item/score can take
#' - Resolution: resolution of the score
#'
#' @export
#' @import dplyr
#'
#' @examples
#' detail_POSCORAD()
#' detail_POSCORAD("Items")
#' detail_POSCORAD(c("B", "C", "oSCORAD", "SCORAD"))
detail_POSCORAD <- function(filter_output = NULL) {

  if (!is.null(filter_output)) {
    stopifnot(is.vector(filter_output, mode = "character"))
  }

  intensity_signs <- c("dryness", "redness", "swelling", "oozing", "scratching", "thickening")
  subjective_symptoms <- c("itching", "sleep")
  dict <- data.frame(Name = c("extent",
                              subjective_symptoms,
                              intensity_signs,
                              "B", "C",
                              "oSCORAD", "SCORAD"),
                     Label = c("Extent",
                               "Itching VAS", "Sleep disturbance VAS",
                               "Dryness", "Redness", "Swelling", "Scabs/Oozing", "Traces of scratching", "Thickening",
                               "Intensity", "Subjective symptoms",
                               "oSCORAD", "SCORAD"),
                     Maximum = c(100, rep(10, 2), rep(3, 6), 18, 20, 83, 103),
                     Resolution = c(1, rep(.1, 2), rep(1, 6), 1, .1, 0.1, 0.1))

  if (is.null(filter_output)) {
    out <- dict
  } else {
    out <- lapply(filter_output,
                  function(x) {
                    if (x %in% dict$Name) {
                      sub <- filter(dict, .data$Name == x)
                    } else if (x == "Intensity signs") {
                      sub <- filter(dict, .data$Name %in% intensity_signs)
                    } else if (x == "Subjective symptoms") {
                      sub <- filter(dict, .data$Name %in% subjective_symptoms)
                    } else if (x == "Items") {
                      sub <- filter(dict, .data$Name %in% c("extent", intensity_signs, subjective_symptoms))
                    } else if (x == "Components") {
                      sub <- filter(dict, .data$Name %in% c("extent", "B", "C"))
                    } else if (x == "Scores") {
                      sub <- filter(dict, .data$Name %in% c("oSCORAD", "SCORAD"))
                    } else {
                      sub <- NULL
                    }
                    return(sub)
                  }) %>%
      bind_rows() %>%
      distinct()
  }

  return(out)
}

# Models ------------------------------------------------------------------
# Artifically importing EczemaPred here so that Roxygen include it in NAMESPACE

#' List available Stan models
#'
#' NB: does not include uniform or historical forecast
#'
#' @param score (optional) score to subset the output
#'
#' @return Dataframe with columns Score and Model
#'
#' @import dplyr EczemaPred
#'
#' @export
available_models <- function(score = "") {

  out <- bind_rows(
    expand_grid(Score = "extent",
                Model = c("RW", "BinRW", "BinMC")),
    expand_grid(Score = c("itching", "sleep", "C"),
                Model = c("RW", "BinRW")),
    expand_grid(Score = detail_POSCORAD("Intensity signs")$Name,
                Model = c("MC", "OrderedRW")),
    expand_grid(Score = "B",
                Model = c("RW", "BinRW")),
    expand_grid(Score = c("SCORAD", "oSCORAD"),
                Model = c("RW", "AR1", "MixedAR1", "Smoothing"))
  )

  if (any(score %in% out[["Score"]])) {
    out <- filter(out, Score %in% score)
  }

  return(out)

}
