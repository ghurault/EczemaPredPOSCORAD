# Notes -------------------------------------------------------------------

# Plot performance at a given iteration (as a pointrange) for all severity items (x axis), models (colour) and dataset (facet)
# Plot learning curve and prediction horizon plot for all severity items

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))

#### OPTIONS
metric <- "lpd" # for comparing learning curves, not paired comparisons
t_horizon <- 4 # horizon that was used for forward chaining
max_horizon <- 14 # restric prediction horizon
pred_horizon <- 4 # prediction horizon
####

metric <- match.arg(metric, c("lpd", "RPS"))
stopifnot(t_horizon > 0,
          max_horizon > 0)

datasets <- c("Derexyl", "PFDC")

list_models <- bind_rows(
  data.frame(Item = "extent",
             Model = c("uniform", "historical", "RW", "BinMC"), # "BinRW"
             Component = "Extent") %>%
    mutate(Label = case_when(Model == "BinMC" ~ "Main model",
                             TRUE ~ "Reference model")),
  expand_grid(Item = c("itching", "sleep"),
              Model = c("uniform", "historical", "RW", "BinRW"),
              Component = "Subjective symptoms") %>%
    mutate(Label = case_when(Model == "BinRW" ~ "Main model",
                             TRUE ~ "Reference model")),
  expand_grid(Item = detail_POSCORAD("Intensity signs")$Name,
              Model = c("uniform", "historical", "MC", "OrderedRW"),
              Component = "Intensity signs") %>%
    mutate(Label = case_when(Model == "OrderedRW" ~ "Main model",
                             TRUE ~ "Reference model"))
) %>%
  expand_grid(.,
              data.frame(Dataset = datasets))

list_models[["File"]] <- vapply(1:nrow(list_models),
                                function(i) {
                                  get_results_files(outcome = list_models$Item[i],
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
                 df <- load_dataset(as.character(list_models$Dataset[i]))
                 fc_it <- detail_fc_training(df %>% rename(Time = Day), t_horizon)
                 readRDS(list_models$File[i]) %>%
                   filter(Horizon <= max_horizon,
                          Iteration > 0 | list_models$Model[i] != "RW") %>%
                   estimate_performance(metric, ., fc_it, adjust_horizon = !(list_models$Model[i] %in% c("historical", "uniform"))) %>%
                   bind_cols(., list_models[i, ])
               }) %>%
  bind_rows()

# Performance at given iteration --------------------------------------------------------------------

### OPTIONS
it_Derexyl <- 19 # iteration to plot the performance for the Derexyl dataset
it_PFDC <- 16 # iteration to plot the performance for the PFDC dataset
###

brk <- c(.01, .1, .25, .5, 1)

tmp <- perf %>%
  filter(Variable == "Fit",
         (Iteration == it_Derexyl & Dataset == "Derexyl") | (Iteration == it_PFDC & Dataset == "PFDC"),
         Horizon == pred_horizon) %>%
  mutate(Label = factor(Label, levels = rev(c("Main model", "Reference model"))),
         Model = factor(Model, levels = c("uniform", "historical", "RW", "MC",
                                              "BinMC", "OrderedRW", "BinRW")),
         Dataset = recode(Dataset, Derexyl = "Dataset 1", PFDC = "Dataset 2"))

pal <- cbbPalette[c(7, 6, 5, 4, 3, 1, 2)]

full_plot <- tmp %>%
  ggplot(aes(x = Item, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model, shape = Label)) +
  facet_grid(cols = vars(Dataset), rows = vars(Component), space = "free", scale = "free") +
  geom_pointrange(position = position_dodge(width = .66), size = 1, fill = "white") +
  scale_colour_manual(values = pal) +
  scale_y_continuous(breaks = log(brk), labels = paste0("log(", brk, ")")) +
  scale_shape_manual(values = c(16, 21)) +
  coord_flip() +
  labs(x = "", y = metric, colour = "", shape = "") +
  theme_bw(base_size = 15) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, vjust = .5, hjust = .5))
full_plot

# Custom legend
legend1 <- tmp %>%
  filter(Label == "Main model") %>%
  ggplot(aes(x = Item, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model)) +
  geom_pointrange(position = position_dodge(width = .66), size = 1, fill = "white", shape = 21) +
  scale_colour_manual(values = pal[-(1:4)], name = "EczemaPred models") +
  labs(x = "", y = metric, colour = "") +
  theme_bw(base_size = 15) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title.position = "top"))
legend1 <- get_legend(legend1)

legend2 <- tmp %>%
  filter(Label == "Reference model") %>%
  ggplot(aes(x = Item, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model)) +
  geom_pointrange(position = position_dodge(width = .66), size = 1, shape = 16) +
  scale_colour_manual(values = pal[1:4], name = "Reference models") +
  labs(x = "", y = metric, colour = "") +
  theme_bw(base_size = 15) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title.position = "top"))
legend2 <- get_legend(legend2)

plot_grid(plot_grid(NULL, legend1, NULL, legend2, NULL, nrow = 1),
          full_plot + theme(legend.position = "none"),
          ncol = 1, rel_heights = c(.1, .9))
# ggsave(here("results", "performance_items.jpg"), width = 13, height = 13, units = "cm", dpi = 300, scale = 2)

# Learning curves and prediction horizon ---------------------------------------------------------

brk <- c(.01, .05, .1, .25, .5, .75, 1)

for (severity_item in unique(list_models[["Item"]])) {

  tmp <- perf %>%
    filter(Item == severity_item) %>%
    mutate(Model = factor(Model, levels = rev(c("uniform", "historical", "RW", "MC", "BinMC", "BinRW", "OrderedRW"))))

  # Learning curves
  pl1 <- lapply(datasets,
                function(dataset) {

                  df <- load_dataset(dataset)
                  fc_it <- detail_fc_training(df %>% rename(Time = Day), t_horizon)
                  id_xbrk2 <- vapply(seq(0, 1, length.out = 10),
                                     function(x) {which.min((x - fc_it$Proportion)^2)},
                                     numeric(1))

                  title <- case_when(dataset == "Derexyl" ~ "Dataset 1",
                                     dataset == "PFDC" ~ "Dataset 2")

                  # Learning curve
                  p1 <- tmp %>%
                    filter(Dataset == dataset,
                           Variable == "Fit",
                           Horizon == pred_horizon) %>%
                    ggplot(aes(x = N, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model, fill = Model)) +
                    geom_point() +
                    geom_line() +
                    geom_ribbon(alpha = 0.5) +
                    scale_colour_manual(values = cbbPalette) +
                    scale_fill_manual(values = cbbPalette) +
                    scale_x_continuous(sec.axis = dup_axis(breaks =  fc_it$N[id_xbrk2],
                                                           labels = fc_it$LastTime[id_xbrk2],
                                                           name = "Training days")) +
                    labs(x = "Number of training observations", y = metric, colour = "", fill = "", title = title) +
                    theme_bw(base_size = 15) +
                    theme(panel.grid.minor.y = element_blank(),
                          legend.position = "top")
                  if (metric == "lpd") {
                    p1 <- p1 +
                      scale_y_continuous(limits = c(NA, 0), breaks = log(brk), labels = paste0("log(", brk, ")"))
                  } else if (metric == "RPS") {
                    p1 <- p1 +
                      coord_cartesian(ylim = c(0, .3))
                  }
                  p1

                })

  # Prediction horizon
  pl2 <- lapply(datasets,
                function(dataset) {

                  title <- case_when(dataset == "Derexyl" ~ "Dataset 1",
                                     dataset == "PFDC" ~ "Dataset 2")

                  p2 <- tmp %>%
                    filter(Dataset == dataset,
                           Variable == "Horizon") %>%
                    ggplot(aes(x = Model, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model)) +
                    geom_pointrange(size = 1.5) +
                    scale_colour_manual(values = cbbPalette) +
                    labs(x = "", y = paste0("Change in ", metric, " with\nincreasing prediction horizon"), colour = "", title = title) +
                    theme_bw(base_size = 15) +
                    theme(legend.position = "bottom")
                  if (metric == "lpd") {
                    ymin <- tmp %>% filter(Variable == "Horizon") %>% summarise(min(Mean)) %>% as.numeric()
                    p2 <- p2 + coord_flip(ylim = c(floor(ymin * 10) / 10, 0))
                  }
                  if (metric == "RPS") {
                    p2 <- p2 + coord_flip(ylim = c(0, NA))
                  }
                  p2

                })

  pl <- c(pl1, pl2)
  # Capitalise first letter of severity_item
  Item <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
               severity_item,
               perl = TRUE)
  # Shared title
  plot_title <- ggdraw() +
    draw_label(Item,
               fontface = "bold",
               size = 20,
               x = .5,
               hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  # Combine plots
  plot_grid(
    plot_title,
    get_legend(pl[[1]]),
    plot_grid(
      plotlist = lapply(pl,
                        function(x) {
                          x + theme(legend.position = "none",
                                    plot.title = element_text(face = "bold"))
                        }),
      ncol = 2, rel_heights = c(.6, .4), align = "v"),
    ncol = 1,
    rel_heights = c(.05, .05, .9))
  ggsave(here("results", paste0("performance_", severity_item, ".jpg")), width = 13, height = 8, units = "cm", dpi = 300, scale = 2.5)

}
