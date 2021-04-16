# Notes -------------------------------------------------------------------

# Combine predictions from different models to compute:
# B (intensity), C (subjective symptoms), SCORAD or oSCORAD
# For B and C, RPS and lpd are computed analytically.
# For SCORAD and oSCORAD, lpd and CRPS are computed from samples

# Additional code to assess where does the variance in SCORAD comes from

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))
library(scoringRules)

#### OPTIONS
score <- "SCORAD"
dataset <- "PFDC"
mdl_name <- "EczemaPred" # name to give to the combined model (eg EczemaPred or Combined***)
t_horizon <- 4
sv <- FALSE # Whether to save results

mdl_A <- data.frame(Item = "extent", Model = "BinMC")
mdl_B <- data.frame(Item = detail_POSCORAD("Intensity signs")$Name, Model = "OrderedRW")
mdl_C <- data.frame(Item = detail_POSCORAD("Subjective symptoms")$Name, Model = "BinRW")
####

dataset <- match.arg(dataset, c("Derexyl", "PFDC"))

mdl_scorad <- bind_rows(mdl_A, mdl_B, mdl_C)
stopifnot(mdl_scorad[["Model"]] %in% c("BinRW", "BinMC", "MC", "RW", "OrderedRW"))

dict <- detail_POSCORAD(c("Scores", "Components"))
score <- match.arg(score, dict[["Name"]])
dict <- dict %>% filter(Name == all_of(score))
score_lbl <- dict[["Label"]]

mdl_scorad[["File"]] <- sapply(1:nrow(mdl_scorad),
                               function(i) {
                                 get_results_files(outcome = mdl_scorad$Item[i],
                                                   model = mdl_scorad$Model[i],
                                                   dataset = dataset,
                                                   val_horizon = t_horizon)$Val
                                 })
stopifnot(all(file.exists(mdl_scorad[["File"]])))
res_file <- get_results_files(outcome = score, model = mdl_name, dataset = dataset, val_horizon = t_horizon)$Val

max_score <- dict[["Maximum"]]

POSCORAD <- load_dataset(dataset)

# Combining signs ----------------------------------------------------------

# Prepare dataset
pred <- POSCORAD %>%
  rename(Time = Day, Score = all_of(score_lbl)) %>%
  select(Patient, Time, Score) %>%
  drop_na() %>%
  mutate(Iteration = get_fc_iteration(Time, t_horizon))

# Compute prediction horizon (horizon for the combined score, not the individual scores)
it <- unique(pred[["Iteration"]])
pred <- lapply(it,
               function(i) {
                 split_fc_dataset(pred, i)$Testing
               }) %>%
  bind_rows() %>%
  select(-LastScore, -LastTime)

# Concatenate predictions for different signs
for (i in 1:nrow(mdl_scorad)) {
  severity_item <- as.character(mdl_scorad$Item[i])
  mdl <- as.character(mdl_scorad$Model[i])
  res <- mdl_scorad$File[i] %>%
    readRDS(.) %>%
    select(Patient, Time, Iteration, Samples) %>%
    change_colnames(., "Samples", paste0(severity_item, "_pred"))
  pred <- left_join(pred, res, by = c("Patient", "Time", "Iteration"))
}
pred <- pred %>% drop_na()

# Compute severity scores
pred <- pred %>%
  # mutate(across(all_of(paste0(mdl_scorad[["Item"]], "_pred")), ~map(.x, sample))) %>%
  mutate(B_pred = pmap(list(redness_pred, dryness_pred, swelling_pred, oozing_pred, scratching_pred, thickening_pred),
                       function(...) {Reduce(`+`, list(...))}),
         C_pred = pmap(list(itching_pred, sleep_pred), `+`),
         oSCORAD_pred = pmap(list(extent_pred, B_pred), function(x, y) {0.2 * x + 3.5 * y}),
         SCORAD_pred = pmap(list(oSCORAD_pred, C_pred), `+`))

# Prepare results dataframe
res <- pred %>%
  rename(Samples = all_of(paste0(score, "_pred"))) %>%
  select(Patient, Time, Score, Horizon, Iteration, Samples)

compute_discrete_metrics_from_samples <- function(res, max_score, reso = 1) {
  # Add lpd and RPS to res by estimating pmf from samples
  #
  # Args:
  # res: Dataframe with columns Score and Samples
  # max_score: Maximum value that the score can take
  # reso: resolution of the score
  #
  # Returns:
  # res with additional columns lpd and RPS

  library(dplyr)

  stopifnot(length(max_score) == 1,
            is.numeric(max_score),
            max_score == round(max_score),
            length(reso) == 1,
            is.numeric(reso),
            reso > 0)
  stopifnot(is.data.frame(res),
            all(c("Samples", "Score") %in% colnames(res)))

  support <- seq(0, max_score, reso)

  prob <- do.call(rbind,
                  lapply(1:nrow(res),
                         function(i) {
                           HuraultMisc::extract_distribution(c(support, res$Samples[i][[1]]), type = "discrete", support = support)$Probability
                         }))
  lpd <- sapply(1:nrow(prob),
                function(i) {
                  log(prob[i, as.integer(res$Score[i] / reso) + 1])
                })
  RPS <- sapply(1:nrow(prob),
                function(i) {
                  compute_RPS(prob[i, ], as.integer(res$Score[i] / reso) + 1)
                })
  res <- res %>%
    mutate(lpd = lpd,
           RPS = RPS)

  return(res)

}

if (score %in% c("B", "C")) {
  res <- compute_discrete_metrics_from_samples(res,
                                               max_score = max_score,
                                               reso = case_when(score == "B" ~ 1,
                                                                score == "C" ~ 0.1))
}

if (score %in% c("SCORAD", "oSCORAD")) {
  res <- res %>%
    mutate(CRPS = crps_sample(res[["Score"]], do.call(rbind, res$Samples)),
           lpd = -logs_sample(res[["Score"]], do.call(rbind, res$Samples)))

  # Replace Inf (or close to Inf) estimates of lpd
  lpd_lb <- -log(max_score * 100) # lower bound (cf. 1/100 probability of uniform forecast)
  lpd_ub <- -log(0.1) # cf. uniform density in interval of width 0.1 (cf. resolution)
  res <- res %>%
    mutate(lpd = replace(lpd, lpd < lpd_lb, lpd_lb),
           lpd = replace(lpd, lpd > lpd_ub, lpd_ub))
}

# Save results
if (sv) {
  saveRDS(res, file = here(res_file))
}

# Where does the variance in SCORAD comes from? ----------------------------

if (FALSE) {

  intensity_signs <- detail_POSCORAD("Intensity signs")$Name

  prop <- pred %>%
    mutate(across(ends_with("_pred"), ~sapply(.x, var))) %>%
    mutate(across(all_of(paste0(c(intensity_signs, "B"), "_pred")), ~(.x * 3.5^2))) %>%
    mutate(extent_pred = extent_pred * 0.2^2) %>%
    mutate(across(ends_with("_pred"), ~(.x / SCORAD_pred))) %>%
    mutate(residuals = 1 -  extent_pred - B_pred - C_pred)

  # Evolution of prop_intensity
  fc_it <- detail_fc_training(POSCORAD %>% rename(Time = Day), t_horizon)
  estimate_performance("B_pred", prop, fc_it) %>%
    filter(Variable == "Fit" & Horizon == 1) %>%
    ggplot(aes(x = Iteration, y = Mean, ymin = Mean - SE, ymax = Mean + SE)) +
    geom_line() +
    geom_point() +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bw(base_size = 15)

  # Simplex plot (distribution of variance decomposition at the observation-level)
  library(Ternary)
  coord <- prop %>%
    select(extent_pred, B_pred, C_pred)
  # jpeg("results/SCORAD_prop_variance.jpg", width = 800, height = 800, quality = 90, pointsize = 25)
  TernaryPlot(alab = "extent", blab = "intensity", clab = "subjective")
  TernaryPoints(coord, col = 'black', pch = 20, cex = .2)
  # dev.off()

  # Mean proportion
  prop %>%
    select(ends_with("_pred")) %>%
    pivot_longer(cols = everything(),
                 names_to = "Component",
                 values_to = "PV") %>%
    group_by(Component) %>%
    summarise(Mean = mean(PV),
              SD = sd(PV),
              SE = SD / sqrt(n())) %>%
    arrange(Mean)

  # Rank importance
  rk <- lapply(1:nrow(coord), function(i) {rank(coord[i, ])}) %>% bind_rows()
  apply(rk, 2, function(x) {mean(x == 3)}) # proportion of being the most important component

}
