# Notes -------------------------------------------------------------------

# Illustrate predictions
# Since we don't retrain the model everyday, it may be more accurate to present the model as an animation...

# 101 = patient 5
# 111 = patient 3

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))
library(gganimate)

#### OPTIONS
score <- "SCORAD"
dataset <- "PFDC"
t_horizon <- 4 # horizon that was used for forward chaining
mdl_name <- "EczemaPred"
patient_id <- 16
####

score <- match.arg(score, c("SCORAD", "oSCORAD"))
dataset <- match.arg(dataset, c("Derexyl", "PFDC"))
mdl_name <- match.arg(mdl_name, c("EczemaPred", "Smoothing", "RW", "AR1", "MixedAR1"))

max_score <- detail_POSCORAD(score)$Maximum

res_file <- get_results_files(outcome = score,
                              model = mdl_name,
                              dataset = dataset,
                              val_horizon = t_horizon,
                              root_dir = here())$Val
stopifnot(file.exists(res_file))

# Processing --------------------------------------------------------------

res <- readRDS(res_file)

df <- load_dataset(dataset) %>%
  rename(Time = Day) %>%
  mutate(Iteration = get_fc_iteration(Time, t_horizon))
pt <- unique(df[["Patient"]])

pred <- res %>%
  filter(Patient == patient_id) %>%
  select(Patient, Time, Horizon, Samples)

obs <- df %>%
  filter(Patient == patient_id) %>%
  select(all_of(c("Patient", "Time", "Date", "Iteration", score))) %>%
  rename(Score = all_of(score))

tmp <- full_join(obs, pred, by = c("Patient", "Time"))

ssi <- lapply(1:nrow(tmp),
              function(i) {
                tryCatch({
                  x <- tmp$Samples[i][[1]]
                  HuraultMisc::extract_distribution(x, parName = "", type = "hdi") %>%
                    mutate(Time = tmp$Time[i] - tmp$Horizon[i],
                           PredTime = tmp$Time[i]) %>%
                    select(-Index, -Variable)
                },
                error = function(e) {
                  NULL
                })
              }) %>%
  bind_rows()
lvl <- sort(unique(ssi[["Level"]]), decreasing = TRUE)

updating_days <- data.frame(Time = 1:max(tmp[["Time"]])) %>%
  mutate(Iteration = get_fc_iteration(Time, t_horizon)) %>%
  group_by(Iteration) %>%
  summarise(LastTime = max(Time)) %>%
  filter(Iteration < max(Iteration)) %>%
  pull(LastTime)

# Plot --------------------------------------------------------------------

p <- ggplot()
# Prediction intervals (cf. fill cannot be an aesthetic with a ribbon)
for (i in 1:length(lvl)) {
  p <- p + geom_ribbon(data = subset(ssi, Level == lvl[i]),
                       aes(x = PredTime, ymin = Lower, ymax = Upper, fill = Level))
}
# Actual trajectory
p <- p +
  geom_path(data = tmp, aes(x = Time, y = Score)) +
  geom_point(data = tmp, aes(x = Time, y = Score, group = seq_along(Time)))
# Format
p <- p +
  scale_fill_gradientn(colours = rev(c("#FFFFFF", RColorBrewer::brewer.pal(n = 6, "Blues")))[-1],
                       limits = c(0, 1), breaks = c(.1, .5, .9)) +
  scale_y_continuous(limits = c(0, max_score), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .05))) +
  labs(x = "Day", y = score, fill = "Confidence level") +
  theme_classic(base_size = 15) +
  theme(legend.position = "top")

# Static image
ps <- p +
  geom_vline(xintercept = updating_days + .5, linetype = "dashed", colour = "grey")
ps
if (FALSE) {
  saveRDS(ps, here("results", paste0(score, "_prediction", t_horizon, "_", dataset, "_", patient_id, ".rds")))
  ggsave(here("results", paste0(score, "_prediction", t_horizon, "_", dataset, "_", patient_id, ".jpg")),
         width = 13, height = 8, units = "cm", dpi = 300, scale = 2)
}

# Animation
if (FALSE) {
  p <- p + transition_reveal(Time)
  p
  anim_save(here("results", paste0(score, "_prediction", t_horizon, "_", dataset, "_", patient_id, ".gif")))
}

# Combine plots -----------------------------------------------------------

if (FALSE) {

  # Derexyl
  # - 51 (cf. pid=53 as example data, cf. absent signs, correlation, stable, good prediction)
  # - 138 (cf. 142, less table but prediction still OK-ish)

  # PFDC
  # - 5 (cf. pid=8 as example data, more dynamic, especially subjective symptoms)
  # - 16 (cf. pid=20); or potentially 13 (cf. pid=17)

  pl_Derexyl <- list(readRDS(here("results/SCORAD_prediction4_Derexyl_51.rds")),
                     readRDS(here("results/SCORAD_prediction4_Derexyl_138.rds")))
  pl_PFDC <- list(readRDS(here("results/SCORAD_prediction4_PFDC_5.rds")),
                  readRDS(here("results/SCORAD_prediction4_PFDC_16.rds")))

  plot_grid(
    plot_grid(pl_Derexyl[[1]] +
                labs(title = "Dataset 1") +
                theme(legend.position = "none",
                      plot.title = element_text(face = "bold")),
              pl_Derexyl[[2]] +
                theme(legend.position = "none"),
              ncol = 1, labels = c("A", "")),
    plot_grid(pl_PFDC[[1]] +
                labs(title = "Dataset 2") +
                theme(legend.position = "none",
                      plot.title = element_text(face = "bold")),
              pl_PFDC[[2]] +
                theme(legend.position = "none"),
              ncol = 1, labels = c("B", "")),
    get_legend(pl_Derexyl[[1]] + labs(fill = "Confidence\nlevel") + theme(legend.position = "right")),
    nrow = 1, rel_widths = c(.45, .45, .1)
  )
  # ggsave(here("results", "prediction_curves.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 2.5)

  # Corresponding trajectories for example data
  plot_grid(plot_POSCORAD("Derexyl", 53),
            plot_POSCORAD("PFDC", 8),
            nrow = 1,
            labels = "AUTO")
  # ggsave(here("results", "data_example.jpg"), width = 13, height = 10, units = "cm", dpi = 300, scale = 3)

}
