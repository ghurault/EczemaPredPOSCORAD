# Notes -------------------------------------------------------------------

# Save posterior and prior summary statistics of the main parameters, for all items

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))

intensity_signs <- detail_POSCORAD("Intensity signs")$Name
subjective_symptoms <- detail_POSCORAD("Subjective symptoms")$Name

####
datasets <- c("Derexyl", "PFDC")
mdl_A <- data.frame(Item = "extent", Model = "BinMC")
mdl_B <- data.frame(Item = intensity_signs, Model = "OrderedRW")
mdl_C <- data.frame(Item = subjective_symptoms, Model = "BinRW")
####

list_models <- bind_rows(mdl_A, mdl_B, mdl_C) %>%
  mutate(PopulationParameter = map(Model, ~list_parameters(.x)$Population),
         PatientParameter = map(Model, ~list_parameters(.x)$Patient))

l_prior <- list_models %>%
  mutate(Distribution = "Prior",
         File = map2(Item, Model, ~get_results_files(outcome = .x, model = .y, root_dir = here())$PriorPar) %>% unlist())

l_post <- list_models %>%
  expand_grid(., Dataset = datasets) %>%
  mutate(Distribution = paste0("Posterior - ", Dataset),
         File = pmap(list(Item, Model, Dataset),
                     function(x, y, z) {
                       get_results_files(outcome = x, model = y, dataset = z, root_dir = here())$FitPar
                     }) %>%
           unlist()) %>%
  select(-Dataset)

list_models <- bind_rows(l_prior, l_post)

stopifnot(all(file.exists(list_models$File)))

# Combine estimates -------------------------------------------------------

par_POSCORAD <- lapply(unique(list_models[["Item"]]),
              function(item) {
                tmp <- list_models %>% filter(Item == item)

                out <- lapply(1:nrow(tmp),
                              function(i) {
                                readRDS(tmp$File[i]) %>%
                                  mutate(Distribution = tmp$Distribution[i],
                                         Model = tmp$Model[i]) %>%
                                  filter(Variable %in% tmp$PopulationParameter[i][[1]] |
                                           (Distribution == "Prior" & Variable %in% tmp$PatientParameter[i][[1]] & Index == 1))
                              }) %>%
                  bind_rows() %>%
                  select(-Patient, -Time) %>%
                  mutate(Distribution = fct_relevel(factor(Distribution), "Prior"),
                         Item = item)

                return(out)
              }) %>%
  bind_rows()

# usethis::use_data(par_POSCORAD, overwrite = TRUE)

# Compare fit -------------------------------------------------------------

# par_POSCORAD <- EczemaPredPOSCORAD::par_POSCORAD

bind_rows(par_POSCORAD %>%
            filter(!is.na(Index)) %>%
            mutate(Variable = paste0(Variable, "[", Index, "]")),
          par_POSCORAD %>%
            filter(is.na(Index))) %>%
  select(-Index) %>%
  ggplot(aes(x = Variable, y = Mean, ymin = `5%`, ymax = `95%`, colour = Distribution)) +
  facet_wrap(vars(Item), scales = "free") +
  geom_pointrange(position = position_dodge(width = .25)) +
  coord_flip() +
  scale_colour_manual(values = cbbPalette) +
  labs(x = "", y = "Estimate", colour = "") +
  theme_bw(base_size = 15) +
  theme(legend.position = "top")
# ggsave(here("results", "compare_fit.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 3)
