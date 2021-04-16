# Notes -------------------------------------------------------------------

# Plot distribuion of each severity item and (o)SCORAD

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))

#### OPTIONS
dataset <- "PFDC"
####

dataset <- match.arg(dataset, c("Derexyl", "PFDC"))

sev_dict <- detail_POSCORAD(c("Items", "Scores"))

df <- load_dataset(dataset)

# Plot --------------------------------------------------------------------

pl <- lapply(1:nrow(sev_dict),
             function(i) {
               symp <- as.character(sev_dict$Name[i])
               lbl <- as.character(sev_dict$Label[i])

               if (symp %in% detail_POSCORAD("Intensity signs")$Name) {
                 p <- table(na.omit(factor(df[[lbl]], levels = 0:sev_dict$Maximum[i])))
                 N <- sum(p)
                 p <- p / N

                 out <- data.frame(p) %>%
                   ggplot(aes(x = Var1, y = Freq)) +
                   geom_col() +
                   scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
                   labs(y = "Frequency", x = "", title = lbl) +
                   theme_bw(base_size = 15)
               } else {
                 out <- df %>%
                   select(all_of(lbl)) %>%
                   drop_na() %>%
                   rename(x = all_of(lbl)) %>%
                   ggplot(aes(x = x)) +
                   geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
                   scale_x_continuous(limits = c(0, sev_dict$Maximum[i])) +
                   scale_y_continuous(limits = c(0, .25), expand = c(0, 0)) +
                   labs(y = "Frequency", x = "", title = lbl) +
                   theme_bw(base_size = 15)
               }
               return(out)
             })


title <- case_when(dataset == "Derexyl" ~ "Dataset 1",
                   dataset == "PFDC" ~ "Dataset 2")

plot_title <- ggdraw() +
  draw_label(title,
             fontface = "bold",
             size = 20,
             x = .5,
             hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))
plot_grid(plot_title,
          plot_grid(plotlist = pl, ncol = 3),
          ncol = 1, rel_heights = c(.05, .95))
# ggsave(here("results", paste0("distribution_", dataset, ".jpg")), width = 13, height = 8, units = "cm", dpi = 300, scale = 3)
