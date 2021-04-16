# Notes -------------------------------------------------------------------

# Prior predictive check and Fake data check for benchmark models predicting SCORAD/oSCORAD

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

set.seed(1744834965) # Reproducibility (Stan use a different seed)

source(here::here("analysis", "00_init.R"))

#### OPTIONS
mdl_name <- "MixedAR1"
score <- "SCORAD"
n_pt <- 5
n_dur <- rpois(n_pt, 50)
run_prior <- FALSE
run_fake <- TRUE
n_chains <- 4
n_it <- 2000
####

score <- match.arg(score, c("SCORAD", "oSCORAD"))
mdl_name <- match.arg(mdl_name, c("RW", "AR1", "MixedAR1", "Smoothing"))
stopifnot(is_scalar_wholenumber(n_pt),
          n_pt > 0,
          all(is_wholenumber(n_dur)),
          all(n_dur > 0),
          is_scalar_logical(run_prior),
          is_scalar_logical(run_fake),
          is_scalar_wholenumber(n_chains),
          n_chains > 0,
          is_scalar_wholenumber(n_it),
          n_it > 0)

max_score <- detail_POSCORAD(score)$Maximum

file_dict <- get_results_files(outcome = score, model = mdl_name)

param <- list_parameters(mdl_name)
param$Test <- NULL

# Prior predictive check -------------------------------------------------

id <- get_index2(n_pt, n_dur)

if (run_prior) {
  fit_prior <- sample_prior_continuous(N_patient = n_pt,
                                       t_max = n_dur,
                                       max_score = max_score,
                                       model = mdl_name,
                                       pars = unlist(param),
                                       iter = n_it,
                                       chains = n_chains)
  saveRDS(fit_prior, file = file_dict$PriorFit)
  par0 <- extract_parameters(fit_prior, pars = param, id = id)
  saveRDS(par0, file = file_dict$PriorPar)
} else {
  fit_prior <- readRDS(file_dict$PriorFit)
  par0 <- readRDS(file_dict$PriorPar)
}

yrep <- rstan::extract(fit_prior, pars = "y_rep")[[1]]

if (FALSE) {

  check_hmc_diagnostics(fit_prior)
  # pairs(fit_prior, pars = param$Population)

  # Prior distribution
  plot(fit_prior, pars = c(param$Population, paste0(c(param$Patient, param$PatientTime), "[1]")), plotfun = "hist")

  yrep1 <- yrep[, id %>% filter(Patient == 1) %>% pull(Index)] # cf. first patient

  # Summary statistics of interest
  # Proportion of well-controlled days (should not change much for different patients)
  ggplot(data = data.frame(x = apply(yrep1, 1, function(x) {mean(x < max_score * 0.1)})),
         aes(x = x)) +
    geom_density(fill = "#9ecae1") +
    labs(x = "Proportion of well-controlled days") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    theme_bw(base_size = 15)
  # Normalised amplitude (can change depending on the time-series length)
  apply(yrep1, 1, function(x) {(max(x) - min(x)) / max_score}) %>%
    data.frame(x = .) %>%
    ggplot(aes(x = x)) +
    geom_density(fill = "#9ecae1") +
    labs(x = "Normalised amplitude") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    theme_bw(base_size = 15)
  # ggsave(paste0("results/prior_amplitude_", score, "_", mdl_name, ".jpg"), width = 7, height = 8, units = "cm", dpi = 300, scale = 1.5)

  # Draw from predictive distribution (first patient)
  lapply(sample(nrow(yrep1), 4),
         function(i) {
           ggplot(data = data.frame(t = 1:n_dur[1],
                                    y = yrep1[i, ]),
                  aes(x = t, y = y)) +
             geom_line() +
             coord_cartesian(ylim = c(0, max_score)) +
             theme_bw(base_size = 15)
         }) %>%
    plot_grid(plotlist = ., ncol = 2)
}

# Fitting fake data ---------------------------------------------------------

p_mis <- .25
p_obs_obs <- .75
horizon <- 5

# Take one draw (different draws corresponds to different a priori pattern in the data)
draw <- 4

true_param <- rstan::extract(fit_prior, pars = unlist(param)) %>% extract_draws(draw)

# Dataframe of fake trajectories
fd <- id %>%
  mutate(Score = yrep[draw, ]) %>%
  select(-Index)

# Add missing values (but not at the beginning and end of the time-series)
fd <- lapply(1:n_pt,
             function(pid) {
               sub_fd <- subset(fd, Patient == pid)
               id_mis <- c(generate_missing(nrow(sub_fd) - horizon, type = "markovchain", p_mis = p_mis, p_obs_obs = p_obs_obs),
                           rep(FALSE, horizon)) # don't generate missing values for prediction horizon
               sub_fd[id_mis, "Score"] <- NA
               return(sub_fd)
             }) %>%
  bind_rows()

# Plot different patients trajectories
lapply(sample(1:n_pt, min(n_pt, 4)),
       function(pid) {
         ggplot(data = fd %>% filter(Patient == pid) %>% drop_na(),
                aes(x = Time, y = Score)) +
           geom_line() +
           geom_point() +
           coord_cartesian(ylim = c(0, max_score)) +
           theme_bw(base_size = 15)
       }) %>%
  plot_grid(plotlist = ., ncol = 2)

# Process data for fitting
fd <- fd %>%
  drop_na() %>%
  group_by(Patient) %>%
  mutate(t_max = max(Time),
         Label = case_when(Time <= t_max - horizon ~ "Training",
                           TRUE ~ "Testing")) %>%
  select(-t_max) %>%
  ungroup()
train <- fd %>% filter(Label == "Training")
test <- fd %>% filter(Label == "Testing")

id <- get_index(train, test)
fd <- left_join(fd, id, by = c("Patient", "Time"))

if (run_fake) {
  fit_fake <- fit_continuous(train = train,
                             test = test,
                             max_score = max_score,
                             model = mdl_name,
                             pars = unlist(param),
                             iter = n_it,
                             chains = n_chains,
                             control = list(adapt_delta = 0.9))
  saveRDS(fit_fake, file = file_dict$FakeFit)
} else {
  fit_fake <- readRDS(file_dict$FakeFit)
}

# Fake data check ---------------------------------------------------------

if (FALSE) {

  check_hmc_diagnostics(fit_fake)

  pairs(fit_fake, pars = param$Population)
  # print(fit_fake, pars = param$Population)

  # Check model sentivity (prior vs posterior)
  par_fake <- extract_parameters(fit_fake, pars = param, id = id)
  HuraultMisc::plot_prior_influence(par0, par_fake, pars = c(param$Population, param$Patient))
  HuraultMisc::plot_prior_posterior(par0, par_fake, pars = param$Population)

  ## Can we recover known parameters?
  tmp <- par_fake %>%
    full_join(true_param, by = c("Variable" = "Parameter", "Index")) %>%
    rename(True = Value)
  # Population parameters
  ggplot(data = subset(tmp, Variable %in% param$Population),
         aes(x = Variable)) +
    geom_pointrange(aes(y = Mean, ymin = `5%`, ymax = `95%`)) +
    geom_point(aes(y = True), col = "#E69F00", size = 2) +
    # scale_y_log10() +
    coord_flip() +
    labs(x = "", y = "Estimate") +
    theme_bw(base_size = 20)

  ## Posterior predictive checks
  patient_ids <- sample(1:n_pt, min(4, n_pt))
  pl5 <- lapply(patient_ids,
                function(pid) {
                  plot_ppc_traj_fanchart(fit_fake, train = train, test = test, patient_id = pid, max_score = max_score) +
                    labs(title = paste("Patient", pid))
                })
  plot_grid(get_legend(pl5[[1]] + theme(legend.position = "top", legend.key.size = unit(1, "cm"))),
            plot_grid(plotlist = lapply(pl5, function(p) {p + theme(legend.position = "none")}), ncol = 2),
            ncol = 1, rel_heights = c(.1, .9))

  yrep_fake <- rstan::extract(fit_fake, pars = "y_rep")[[1]]
  # Coverage of the posterior predictive distribution
  HuraultMisc::plot_coverage(yrep_fake[, fd[["Index"]]], fd[["Score"]])
  # Posterior predictive p-value for well-controlled-days (averaged across-patients)
  post_pred_pval(yrep_fake[, fd[["Index"]]], fd[["Score"]], function(x) {mean(x < max_score * 0.1, na.rm = TRUE)}, plot = TRUE)
  # Posterior predictive distribution for normalised amplitude
  lapply(sample(1:n_pt, 4),
         function(pid) {
           tmp <- fd %>%
             filter(Patient == pid)
           post_pred_pval(yrep[, tmp[["Index"]]], tmp[["Score"]], function(x) {(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / max_score},
                          plot = TRUE)$plot +
             coord_cartesian(xlim = c(0, 1)) +
             labs(x = "Normalised amplitude")
         }) %>%
    plot_grid(plotlist = ., ncol = 2)

}
