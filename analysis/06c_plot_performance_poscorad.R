# Notes -------------------------------------------------------------------

# Plot SCORAD learning curves for different models (colour), dataset (column facet), metric(row facet)
# facet no possible if we want to have a secondary x axis (Iteration) that depends on the dataset, or control the y-axis range
# have the main model in the foreground

# Plot SCORAD prediction horizon for different models, dataset and metrics

# perf %>% filter(Model == "EczemaPred", Metric == "Accuracy", Mean >= 0.7) %>% group_by(Horizon) %>% filter(Iteration == min(Iteration))

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))

#### OPTIONS
score <- "oSCORAD"
t_horizon <- 4 # horizon that was used for forward chaining
pred_horizon <- 4 # prediction horizon
max_horizon <- 14 # restric prediction horizon
acc_thr <- 5 # accuracy threshold
####

score <- match.arg(score, c("SCORAD", "oSCORAD"))
stopifnot(t_horizon > 0,
          pred_horizon > 0,
          max_horizon > pred_horizon)

mdl_names <- c("uniform", "historical", "RW", "AR1", "MixedAR1", "Smoothing", "EczemaPred")
datasets <- c("Derexyl", "PFDC")
metrics <- c("lpd", "Accuracy")

list_models <- expand_grid(Model = mdl_names, Dataset = datasets, Metric = metrics)
list_models[["File"]] <- vapply(1:nrow(list_models),
                                function(i) {
                                  get_results_files(outcome = score,
                                                    model = list_models$Model[i],
                                                    dataset = list_models$Dataset[i],
                                                    val_horizon = t_horizon,
                                                    root_dir = here())$Val
                                },
                                character(1))
stopifnot(all(file.exists(list_models$File)))

# Processing --------------------------------------------------------------

perf <- lapply(1:nrow(list_models),
               function(i) {
                 dataset <- list_models$Dataset[i]
                 metric <- list_models$Metric[i]
                 mdl_name <- list_models$Model[i]
                 res_file <- list_models$File[i]

                 df <- load_dataset(as.character(dataset))
                 fc_it <- detail_fc_training(df %>% rename(Time = Day), t_horizon)

                 res <- readRDS(res_file)
                 if (metric == "Accuracy") {
                   res <- res %>% mutate(Accuracy = compute_accuracy(res[["Score"]], res[["Samples"]], acc_thr))
                 }
                 res %>%
                   filter(Horizon <= max_horizon) %>%
                   estimate_performance(metric, ., fc_it, adjust_horizon = !(mdl_name %in% c("historical", "uniform"))) %>%
                   bind_cols(list_models[i, ])
               }) %>%
  bind_rows() %>%
  mutate(Model = factor(Model, levels = mdl_names))

it <- list_models %>% select(Dataset, Metric) %>% distinct() %>% arrange(desc(Metric), Dataset)

# Learning curves -------------------------------------------------------------------

pl <- lapply(1:nrow(it),
             function(dm) {

               df <- load_dataset(as.character(it$Dataset[dm]))
               fc_it <- detail_fc_training(df %>% rename(Time = Day), t_horizon)

               id_xbrk2 <- sapply(seq(0, 1, length.out = 10), function(x) {which.min((x - fc_it$Proportion)^2)})

               tmp <- left_join(it[dm, ], list_models, by = c("Dataset", "Metric"))
               metric <- it$Metric[dm]
               dataset <- it$Dataset[dm]
               title <- case_when(dataset == "Derexyl" ~ "Dataset 1",
                                  dataset == "PFDC" ~ "Dataset 2")

               p1 <- perf %>%
                 filter(Variable == "Fit",
                        Metric == metric,
                        Dataset == dataset,
                        Horizon == pred_horizon) %>%
                 ggplot(aes(x = N, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model, fill = Model)) +
                 geom_point() +
                 geom_line() +
                 geom_ribbon(alpha = 0.5) +
                 scale_colour_manual(values = rev(cbbPalette[1:length(mdl_names)]), guide = guide_legend(reverse = TRUE)) +
                 scale_fill_manual(values = rev(cbbPalette[1:length(mdl_names)]), guide = guide_legend(reverse = TRUE)) +
                 scale_x_continuous(sec.axis = dup_axis(breaks = fc_it$N[id_xbrk2],
                                                        labels = fc_it$LastTime[id_xbrk2],
                                                        name = "Training days")) +
                 labs(x = "Number of training observations", y = metric, colour = "", fill = "", title = title) +
                 theme_bw(base_size = 15) +
                 theme(panel.grid.minor.y = element_blank(),
                       plot.title = element_text(face = "bold"),
                       legend.position = "top")
               if (metric == "lpd") {
                 p1 <- p1 +
                   coord_cartesian(ylim = c(-5, -.5)) # set manually so that Derexyl and PFDC are aligned
               }
               if (metric == "CRPS") {
                 p1 <- p1 +
                   scale_y_continuous(limits = c(0, NA))
               }
               if (metric == "Accuracy") {
                 p1 <- p1 +
                   scale_y_continuous(breaks = seq(0, 1, .1), expand = c(0, 0)) +
                   coord_cartesian(ylim = c(0, 1))
               }
               return(p1)
             })

plot_grid(get_legend(pl[[1]] + theme(legend.position = "top")),
          plot_grid(plotlist = lapply(pl, function(x) {x + theme(legend.position = "none")}),
                    ncol = 2),
          ncol = 1, rel_heights = c(.1, .9))
# ggsave(here("results", paste0("learning_curves_", score, ".jpg")), width = 13, height = 8, units = "cm", dpi = 300, scale = 2.5)

# Prediction horizon ------------------------------------------------------

perf %>%
  filter(Variable == "Horizon") %>%
  mutate(Model = factor(Model, levels = rev(levels(Model))),
         Dataset = recode(Dataset, Derexyl = "Dataset 1", PFDC = "Dataset 2")) %>%
  ggplot(aes(x = Model, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model)) +
  facet_grid(cols = vars(Metric), rows = vars(Dataset), scales = "free_x") +
  geom_pointrange(size = 1.5) +
  coord_flip() +
  scale_colour_manual(values = cbbPalette) +
  labs(x = "", y = paste0("Change in performance with \nincreasing prediction horizon"), colour = "", title = score) +
  theme_bw(base_size = 15) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))
# ggsave(here("results", paste0("horizon_", score, ".jpg")), width = 13, height = 8, units = "cm", dpi = 300, scale = 2.5)
