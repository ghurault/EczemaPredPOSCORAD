# Notes -------------------------------------------------------------------

# Generate fake data in a similar format as the Derexyl/PFDC datasets by sampling the prior predictive distribution

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))
library(foreach)
library(doParallel)

set.seed(1744834965) # Reproducibility (Stan use a different seed)

#### OPTIONS
n_pt <- 16 # Number of patients
n_dur <- rpois(n_pt, 80) # Time-series duration for each patient
p_mis <- 0.1 # Proportion of missing values
p_obs_obs <- 0.9 # Probability that the next value is observed when current value is observed

dgp <- detail_POSCORAD("Items") %>%
  mutate(Model = case_when(Name %in% c("extent") ~ "BinMC",
                           Name %in% c("itching", "sleep") ~ "BinRW",
                           TRUE ~ "OrderedRW")) # Model for each item

n_chains <- 4
n_it <- 2000
####

dgp <- dgp %>%
  mutate(M = Maximum / Resolution) %>%
  group_by(Model, M) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

id <- get_index2(n_dur)

# Missing values
id <- lapply(1:n_pt,
             function(pid) {
               id %>%
                 filter(Patient == pid) %>%
                 mutate(Missing = generate_missing(nrow(.), type = "markovchain", p_mis = p_mis, p_obs_obs = p_obs_obs))
             }) %>%
  bind_rows()

# Processing --------------------------------------------------------------

runs <- dgp %>%
  select(ID, Model, M) %>%
  distinct()

cl <- makeCluster(nrow(runs), outfile = "")
registerDoParallel(cl)

out <- foreach(i = 1:nrow(runs), .combine = c) %dopar% {

  source(here::here("analysis", "00_init.R"))

  dgp_i <- dgp %>% filter(ID == i)

  model <- EczemaModel(runs$Model[i], max_score = runs$M[i], discrete = TRUE)
  param <- list_parameters(model)

  fit_prior <- sample_prior(model,
                            N_patient = n_pt,
                            t_max = n_dur,
                            pars = unlist(param),
                            iter = n_it,
                            chains = n_chains)

  draws <- sample(n_it * n_chains / 2, nrow(dgp_i), replace = FALSE)

  out <- lapply(1:nrow(dgp_i),
         function(j) {

           sim <- extract_simulations(fit_prior, id = id, draw = draws[j], pars = unlist(param[c("Population", "Patient")]))
           df <- sim$Data %>%
             mutate(Score = Score * dgp_i$Resolution[j],
                    Score = replace(Score, Missing, NA)) %>%
             rename(setNames("Score", dgp_i$Label[j])) %>%
             select(-Index, -Missing)

           par <- dgp_i[j, ] %>%
             select(Model, Label) %>%
             rename(Item = Label) %>%
             bind_cols(sim$Parameters)

           out <- list(Data = df, Parameters = par)
           return(out)

         })
  return(out)

}

stopCluster(cl)

# Save data ---------------------------------------------------------------

df <- lapply(out, function(x) {x$Data}) %>%
  Reduce(function(x, y) {full_join(x, y, by = c("Patient", "Time"))}, .) %>%
  rename(Day = Time) %>%
  mutate(Intensity = Redness + Dryness + `Traces of scratching` + Thickening + Swelling + `Scabs/Oozing`,
         `Subjective symptoms` = `Sleep disturbance VAS` + `Itching VAS`,
         oSCORAD = 0.2 * Extent + 3.5 * Intensity,
         SCORAD = oSCORAD + `Subjective symptoms`)

par <- lapply(out, function(x) {x$Parameters}) %>% bind_rows()

FakeData <- list(Data = df, Parameters = par)

# usethis::use_data(FakeData, overwrite = TRUE)

# Plot data ---------------------------------------------------------------

# df <- FakeData$Data

lapply(sort(sample(1:n_pt, min(n_pt, 4))),
       function(pid) {
         df %>%
           filter(Patient == pid) %>%
           drop_na() %>%
           ggplot(aes(x = Day, y = SCORAD)) +
           geom_line() +
           geom_point() +
           coord_cartesian(ylim = c(0, 103)) +
           labs(colour = "", title = paste0("Patient ", pid)) +
           theme_bw(base_size = 15) +
           theme(legend.position = "top")
       }) %>%
  plot_grid(plotlist = ., ncol = 2)
