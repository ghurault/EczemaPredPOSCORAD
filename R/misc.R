# Models ------------------------------------------------------------------

# artifically importing EczemaPred here for NAMESPACE

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
