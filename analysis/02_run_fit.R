# Notes -------------------------------------------------------------------

# Master file to fit models for different severity items and scores
# - Extent: RW, BinRW, BinMC
# - Intensity signs: MC, OrderedRW
# - Subjective symptoms (and B and C): RW, BinRW
# - SCORAD and oSCORAD: RW, AR1, MixedAR1 and Smoothing

# NB: for itching, sleep and C, the resolution is reso=0.1 (there are decimal values) and so the score are multiplied by 1 / reso ...
# ... to work with integers. Replications (and some parameters) must be scaled by reso.

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

set.seed(1744834965) # Reproducibility (Stan use a different seed)

source(here::here("analysis", "00_init.R"))

#### OPTIONS
mdl_name <- "BinMC"
score <- "extent"
dataset <- "PFDC"
run <- TRUE
n_chains <- 4
n_it <- 2000
####

item_dict <- detail_POSCORAD()
score <- match.arg(score, item_dict[["Name"]])
intensity_signs <- detail_POSCORAD("Intensity signs")$Name
dataset <- match.arg(dataset, c("Derexyl", "PFDC"))
mdl_name <- match.arg(mdl_name, available_models(score)$Model)
stopifnot(is_scalar_logical(run),
          is_scalar_wholenumber(n_chains),
          n_chains > 0,
          is_scalar_wholenumber(n_it),
          n_it > 0)
is_continuous <- (score %in% c("SCORAD", "oSCORAD"))

item_dict <- item_dict %>% filter(Name == score)
item_lbl <- as.character(item_dict[["Label"]])
max_score <- item_dict[["Maximum"]]
reso <- item_dict[["Resolution"]]

file_dict <- get_results_files(outcome = score, model = mdl_name, dataset = dataset)

param <- list_parameters(mdl_name)
param[c("Test", "Misc")] <- NULL
if (mdl_name == "BinMC") {
  param$PatientTime <- c("ss1", "lambda", "y_rep")
}

# Data --------------------------------------------------------------------

POSCORAD <- load_dataset(dataset)

# Subset dataset
df <- POSCORAD %>%
  # filter(Patient %in% 1:30) %>%
  rename(Time = Day, Score = all_of(item_lbl)) %>%
  select(Patient, Time, Score) %>%
  drop_na()

pt <- unique(df[["Patient"]])

id <- get_index(df)
df <- left_join(df, id, by = c("Patient", "Time"))

# Fitting -----------------------------------------------------------------

if (run) {
  if (mdl_name == "MC") {
    df_MC <- df %>%
      rename(y = Score) %>%
      group_by(Patient) %>%
      mutate(y0 = y + 1, y1 = lead(y0), dt = lead(Time) - Time) %>%
      ungroup() %>%
      select(y0, y1, dt) %>%
      drop_na()

    fit <- fit_MC(train = df_MC,
                  K = max_score + 1,
                  pars = unlist(param),
                  iter = n_it,
                  chains = n_chains,
                  init = 0)

    id <- NULL

  } else if (is_continuous) {

    fit <- fit_continuous(train = df,
                          max_score = max_score,
                          model = mdl_name,
                          pars = unlist(param),
                          iter = n_it,
                          chains = n_chains,
                          control = list(adapt_delta = .9))

  } else {

    fit <- fit_discrete(train = df %>% mutate(Score = round(Score / reso)),
                        max_score = round(max_score / reso),
                        model = mdl_name,
                        pars = unlist(param),
                        iter = n_it,
                        chains = n_chains,
                        control = list(adapt_delta = .9))

  }

  saveRDS(fit, file = file_dict$Fit)
  par <- extract_parameters(fit, pars = param, id = id)
  saveRDS(par, file = file_dict$FitPar)
}
