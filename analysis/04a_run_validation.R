# Notes -------------------------------------------------------------------

# Run validation for severity items and score predictions

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

set.seed(1744834965) # Reproducibility (Stan use a different seed)

source(here::here("analysis", "00_init.R"))
library(foreach)
library(doParallel)

#### OPTIONS
mdl_name <- "BinMC"
score <- "extent"
dataset <- "PFDC"

run <- FALSE
t_horizon <- 4
n_chains <- 4
n_it <- 2000
n_cluster <- 2
####

item_dict <- detail_POSCORAD()
score <- match.arg(score, item_dict[["Name"]])
dataset <- match.arg(dataset, c("Derexyl", "PFDC", "Fake"))
mdl_name <- match.arg(mdl_name, c("uniform", "historical", available_models(score)$Model))
stopifnot(is_scalar_logical(run),
          is_scalar_wholenumber(n_chains),
          n_chains > 0,
          is_scalar_wholenumber(n_it),
          n_it > 0,
          is_scalar_wholenumber(t_horizon),
          t_horizon > 0,
          is_scalar_wholenumber(n_cluster),
          between(n_cluster, 1, floor((parallel::detectCores() - 2) / n_chains)))

is_continuous <- (score %in% c("SCORAD", "oSCORAD"))
is_stanmodel <- !(mdl_name %in% c("uniform", "historical"))

item_dict <- item_dict %>% filter(Name == score)
item_lbl <- as.character(item_dict[["Label"]])
max_score <- item_dict[["Maximum"]]
reso <- case_when(is_continuous ~ 1,
                  TRUE ~ item_dict[["Resolution"]])
M <- round(max_score / reso)

file_dict <- get_results_files(outcome = score,
                               model = mdl_name,
                               dataset = dataset,
                               val_horizon = t_horizon,
                               root_dir = here())

if (is_continuous) {
  param <- c("lpd", "y_pred")
} else {
  param <- c("lpd", "cum_err", "y_pred")
}

# Data ---------------------------------------------------------------------

POSCORAD <- load_dataset(dataset)

# Subset dataset
df <- POSCORAD %>%
  # filter(Patient %in% 1:30) %>%
  rename(Time = Day, Score = all_of(item_lbl)) %>%
  select(Patient, Time, Score) %>%
  drop_na()

pt <- unique(df[["Patient"]])

# Forward chaining --------------------------------------------------------

df <- df %>% mutate(Iteration = get_fc_iteration(Time, t_horizon))
train_it <- get_fc_training_iteration(df[["Iteration"]])

if (run) {

  cl <- makeCluster(n_cluster, outfile = "")
  registerDoParallel(cl)

  dir.create(file_dict$ValDir)

  out <- foreach(i = rev(seq_along(train_it))) %dopar% {
    it <- train_it[i]

    # Need to reload functions and libraries
    source(here::here("analysis", "00_init.R"))

    duration <- Sys.time()
    cat(glue::glue("Starting iteration {it}"), sep = "\n")

    ####

    split <- split_fc_dataset(df, it)
    train <- split$Training
    test <- split$Testing

    if (mdl_name != "MC") {
      # Scale by reso for fitting (not saving)
      if (is_continuous) {
        train_tmp <- train
        test_tmp <- test
      } else {
        train_tmp <- mutate(train, Score = round(Score / reso))
        test_tmp <- mutate(test, Score = round(Score / reso))
      }
    }

    # Uniform forecast
    if (mdl_name == "uniform") {
      perf <- test_tmp %>%
        add_uniform_pred(test = .,
                         max_score = M,
                         discrete = !is_continuous,
                         include_samples = is_continuous,
                         n_samples = 2 * M) %>%
        mutate(Score = Score * reso)
    }

    # Historical forecast
    if (mdl_name == "historical") {
      perf <- test_tmp %>%
        add_historical_pred(test = .,
                            train = train_tmp,
                            max_score = M,
                            discrete = !is_continuous,
                            add_uniform = TRUE,
                            include_samples = is_continuous) %>%
        mutate(Score = Score * reso)
    }

    # Markov Chain
    if (mdl_name == "MC") {

      train_MC <- train %>%
        rename(y0 = Score) %>%
        group_by(Patient) %>%
        mutate(y0 = y0 + 1, y1 = lead(y0), dt = lead(Time) - Time) %>%
        ungroup() %>%
        select(y0, y1, dt) %>%
        drop_na()

      test_MC <- test %>%
        rename(y0 = LastScore, y1 = Score, dt = Horizon) %>%
        mutate(y0 = y0 + 1, y1 = y1 + 1) %>%
        select(y0, y1, dt)

      model <- EczemaModel("MC", K = max_score + 1)
      fit <- EczemaFit(model,
                       train = train_MC,
                       test = test_MC,
                       iter = n_it,
                       chains = n_chains,
                       init = 0,
                       pars = c("lpd", "cum_err", "y_pred"),
                       refresh = 0)

      perf <- test %>%
        add_predictions(fit = fit, discrete = TRUE, include_samples = TRUE) %>%
        mutate(Samples = map(Samples, ~(.x - 1))) # -1 to be between 0 and max_score

    }

    # Other time-series model (parametrised similarly)
    if (is_stanmodel && mdl_name != "MC") {

      model <- EczemaModel(mdl_name, max_score = M, discrete = !is_continuous)
      fit <- EczemaFit(model,
                       train = train_tmp,
                       test = test_tmp,
                       iter = n_it,
                       chains = n_chains,
                       # control = list(adapt_delta = 0.9),
                       pars = param,
                       refresh = 0)

      perf <- test %>%
        add_predictions(fit = fit, discrete = !is_continuous, include_samples = TRUE) %>%
        mutate(Samples = map(Samples, ~(.x * reso)))

    }

    perf <- perf %>%
      select(-LastTime, -LastScore)

    # Save results (better to save in the loop in case something breaks)
    saveRDS(perf, file = here(file_dict$ValDir, paste0("val_", it, ".rds")))

    ####

    duration <- Sys.time() - duration
    cat(glue::glue("Ending iteration {it} after {round(duration, 1)} {units(duration)}"), sep = "\n")

    # Return
    NULL
  }
  stopCluster(cl)

  # Recombine results
  files <- list.files(file_dict$ValDir, full.names = TRUE)
  if (length(files) < length(train_it)) {
    warning(glue::glue("Number of files (={length(files)}) less than the number of unique iterations (={length(train_it)}).
                       Some runs may have failed."))
  }
  res <- lapply(files,
                function(f) {
                  readRDS(here(f))
                }) %>%
    bind_rows()
  saveRDS(res, file = file_dict$Val)

}
