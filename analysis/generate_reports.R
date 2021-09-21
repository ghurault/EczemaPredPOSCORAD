# Notes -------------------------------------------------------------------

#

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

source(here::here("analysis", "00_init.R"))
library(foreach)
library(doParallel)

render_fit <- FALSE
render_perf <- FALSE

render_parallel <- function(input, rpt, ...) {

  n_cluster <- min(nrow(rpt), parallel::detectCores() - 2)
  cl <- makeCluster(n_cluster, outfile = "")
  registerDoParallel(cl)

  foreach(i = 1:nrow(rpt)) %dopar% {

    tryCatch({
      tf <- tempfile()
      dir.create(tf)
      rmarkdown::render(
        input = input,
        params = rpt$Parameter[[i]],
        output_file = rpt$OutputFile[i],
        output_dir = here::here("docs"),
        intermediates_dir = tf,
        ...
      )
      unlink(tf)
    }, error = function(e) {
      cat(glue::glue("Error in rpt row {i}"), sep = "\n")
    })

    NULL
  }

  stopCluster(cl)

  NULL

}

# Fit ---------------------------------------------------------------------

if (render_fit) {

  rpt <- available_models() %>%
    expand_grid(Dataset = c("Derexyl", "PFDC", "Fake")) %>%
    mutate(Parameters = pmap(list(score = Score, dataset = Dataset, model = Model), list),
           OutputFile = glue::glue("fit_{Score}_{Model}_{Dataset}.html"))

  render_parallel(input = here::here("analysis", "03_check_fit.Rmd"), rpt = rpt, quiet = TRUE)

}

# Performance -------------------------------------------------------

if (render_perf) {

  rpt <- expand_grid(score = detail_POSCORAD()$Name,
                     dataset = c("Derexyl", "PFDC", "Fake"),
                     t_horizon = 4) %>%
    mutate(Parameters = pmap(list(score = score, dataset = dataset, t_horizon = t_horizon), list),
           OutputFile = glue::glue("perf{t_horizon}_{score}_{dataset}.html"))

  render_parallel(input = here::here("analysis", "05_check_performance.Rmd"), rpt = rpt, quiet = TRUE)

}
