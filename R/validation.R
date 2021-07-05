# Global variables
Patient <- NULL
Time <- NULL
Score <- NULL
N <- NULL
Metric <- NULL
Model <- NULL
Mean <- NULL
SD <- NULL
SE <- NULL
Horizon <- NULL

# Metrics -----------------------------------------------------------------

#' Compute accuracy and quantile error
#'
#' - "Accuracy" is defined as the probability that absolute error less than specified value
#' - "Quantile error" is defined as the quantile p of the predictive error distribution
#'
#' @param y vector of outcomes
#' @param pred list of same size of y containing vector of samples
#' @param ct cut-off
#' @param p probability value
#'
#' @return Vector of accuracy values
#'
#' @details
#' The functions are designed to take a vector of outcomes and a list of samples, so they can be used with a dataframe.
#' An alternative would be to use `purrr::map2` in `mutate` directly, and define functions that take a scalar for the outcome
#' and a vector for the samples:
#' e.g. `map2(Score, Samples, ~mean(abs(.x - .y) < acc_thr)) %>% unlist()`
#'
#' @name custom_metrics
NULL

#' @rdname custom_metrics
#' @export
compute_accuracy <- function(y, pred, ct) {

  stopifnot(is.vector(y, mode = "numeric"),
            is.list(pred),
            all(vapply(pred, function(x) {is.vector(x, mode = "numeric")}, logical(1))),
            length(y) == length(pred),
            is_scalar_double(ct),
            ct > 0)

  vapply(
    seq_along(y),
    function(i) {
      mean(abs(y[i] - pred[[i]]) <= ct)
    },
    numeric(1)
  )
}

#' @rdname custom_metrics
#' @export
compute_quantile_error <- function(y, pred, p) {

  stopifnot(is.vector(y, mode = "numeric"),
            is.list(pred),
            all(vapply(pred, function(x) {is.vector(x, mode = "numeric")}, logical(1))),
            length(y) == length(pred),
            is_scalar_double(p),
            p > 0 & p < 1)

  vapply(
    seq_along(y),
         function(i) {
           as.numeric(stats::quantile(abs(y[i] - pred[[i]]), probs = p))
         },
    numeric(1)
    )
}

#' Compute skill scores
#'
#' By default, the skill score is just the metric minus the metric of the reference model.
#' If `metric` is (C)RPS or CRPS, the (C)RPSS is computed.
#'
#' @param res Dataframe of predictions (with different models)
#' @param ref_mdl Name of the reference model
#' @param metrics Names of metrics to normalise by the reference model
#'
#' @return Dataframe res with additional columns corresponding to the skill scores
#' (name metric + suffix "_diff" or (C)RPSS if metric is (C)RPS)
#'
#' @import dplyr
#' @export
compute_skill_scores <- function(res, ref_mdl, metrics) {

  id_vars <- c("Patient", "Time", "Score", "Horizon", "Iteration")

  stopifnot(is.data.frame(res),
            is_scalar_character(ref_mdl),
            is.vector(metrics, mode = "character"),
            all(c(id_vars, metrics) %in% colnames(res)))

  res0 <- res %>%
    filter(.data$Model == ref_mdl) %>%
    select(all_of(c(id_vars, metrics))) %>%
    rename_with(~case_when(.x %in% metrics ~ paste0(.x, "0"),
                           TRUE ~ .x))

  res <- full_join(res,
                   res0,
                   by = id_vars)
  for (x in metrics) {
    if (x %in% c("RPS", "CRPS")) {
      res[[paste0(x, "S")]] <- 1 - res[[x]] / res[[paste0(x, "0")]]
    } else {
      res[[paste0(x, "_diff")]] <- res[[x]] - res[[paste0(x, "0")]]
    }
  }

  return(res)
}

# Estimate performance ----------------------------------------------------

#' Estimate performance (learning curve) with a meta-model
#'
#' The meta-model is a Generative Additive Mixed effect model
#'
#' @param metric_name Name of the metric to estimate the learning curve for
#' @param df Dataframe of validation results
#' @param fc_it Forward chaining iteration characteristics
#' @param adjust_horizon Whether to adjust for prediction horizon in the model
#'
#' @return Dataframe
#'
#' @import dplyr tidyr
#'
#' @export
estimate_performance <- function(metric_name, df, fc_it, adjust_horizon = TRUE) {

  stopifnot(is.character(metric_name),
            is.data.frame(df),
            all(c(metric_name, "Iteration", "Horizon") %in% colnames(df)),
            is.data.frame(fc_it),
            all(c("Iteration", "N", "Proportion") %in% colnames(fc_it)),
            is_scalar_logical(adjust_horizon))

  id_mis <- is.na(df[[metric_name]]) | abs(df[[metric_name]]) == Inf
  if (any(id_mis)) {
    warning("observations with NA or Inf in ", metric_name, " were removed")
    df <- df[!id_mis, ]
  }

  df <- left_join(df, fc_it, by = "Iteration")

  f <- paste0(metric_name, " ~  s(N, bs = 'cr')")
  if (adjust_horizon) {
    f <- paste0(f, " + HorizonMinus1")
  }
  f <- stats::formula(f)

  perf_fit <- expand_grid(fc_it,
                          HorizonMinus1 = 0:(max(df[["Horizon"]]) - 1),
                          Patient = 0)

  perf_mdl <- gamm4::gamm4(f, random = ~ (1 | Patient),
                    data = df %>% mutate(HorizonMinus1 = Horizon - 1))
  s <- summary(perf_mdl$gam)

  # Change in performance when prediction horizon is increased by one day
  if (adjust_horizon) {
    pred_horizon <- data.frame(Mean = s$p.coeff[["HorizonMinus1"]],
                               SE = s$se[["HorizonMinus1"]],
                               Variable = "Horizon")
  } else {
    pred_horizon <- data.frame(Mean = 0,
                               SE = 0,
                               Variable = "Horizon")
  }

  # Mean fit (fixed effect, cf. population mean); the SE correspond to the GAM estimates and don't include the distribution of random effects
  tmp <- stats::predict(perf_mdl$gam, newdata = perf_fit, se.fit = TRUE)
  perf_fit <- perf_fit %>%
    rename(Horizon = .data$HorizonMinus1) %>%
    mutate(Mean = tmp$fit, SE = tmp$se.fit, Variable = "Fit", Horizon = Horizon + 1)

  return(bind_rows(pred_horizon, perf_fit))
}

# Plots -------------------------------------------------------------------

#' Plot learning curves
#'
#' @param perf Dataframe of performance estimate
#' @param metric (optional) name of the metric to plot to set the scale and label.
#' When `metric = "lpd"`, the x-axis is labelled in the context of a discrete forecast.
#' @param fc_it (optional) Dataframe output from `detail_fw_training` to make a secondary x axis
#'
#' @return Ggplot
#'
#' @import ggplot2
#' @export
plot_learning_curves <- function(perf, metric = NULL, fc_it = NULL) {

  stopifnot(is.data.frame(perf),
            all(c("N", "Mean", "SE", "Model") %in% colnames(perf)))

  mdl_names <- intersect(levels(perf[["Model"]]), unique(perf[["Model"]]))

  p1 <- ggplot(data = perf,
               aes(x = N, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model, fill = Model)) +
    geom_point() +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    scale_colour_manual(values = rev(HuraultMisc::cbbPalette[1:length(mdl_names)]), guide = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = rev(HuraultMisc::cbbPalette[1:length(mdl_names)]), guide = guide_legend(reverse = TRUE)) +
    labs(x = "Number of observations", y = metric, colour = "", fill = "") +
    theme_bw(base_size = 15) +
    theme(panel.grid.minor.y = element_blank(),
          legend.position = "top")

  if (!is.null(fc_it)) {
    stopifnot(is.data.frame(fc_it),
              all(c("N", "LastTime", "Proportion") %in% colnames(fc_it)))
    id_xbrk2 <- vapply(seq(0, 1, length.out = 10), function(x) {which.min((x - fc_it$Proportion)^2)}, numeric(1))
    p1 <- p1 +
      scale_x_continuous(sec.axis = dup_axis(breaks =  fc_it$N[id_xbrk2],
                                             labels = fc_it$LastTime[id_xbrk2],
                                             name = "Training day"))
  }

  if (!is.null(metric)) {
    if (metric == "lpd") {
      brk <- c(.01, .05, .1, .25, .5, .75, 1)
      p1 <- p1 +
        scale_y_continuous(breaks = log(brk), labels = paste0("log(", brk, ")")) +
        coord_cartesian(ylim = c(NA, 0))
    } else if (metric %in% c("RPS", "CRPS", "QE")) {
      p1 <- p1 +
        coord_cartesian(ylim = c(0, NA))
    } else if (metric == "Accuracy") {
      p1 <- p1 +
        scale_y_continuous(breaks = seq(0, 1, .1), expand = c(0, 0)) +
        coord_cartesian(ylim = c(0, 1))
    }
  }

  return(p1)

}

#' Plot performance change as prediction horizon is increased
#'
#' @param perf Dataframe of performance estimates
#'
#' @return Ggplot
#'
#' @import ggplot2
#'
#' @export
plot_horizon_change <- function(perf) {

  stopifnot(is.data.frame(perf),
            all(c("N", "Mean", "SE", "Model") %in% colnames(perf)))

  mdl_names <- levels(perf[["Model"]])

  p2 <- ggplot(data = perf,
               aes(x = Model, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model)) +
    geom_pointrange(size = 1.5) +
    scale_colour_manual(values = rev(HuraultMisc::cbbPalette[1:length(mdl_names)])) +
    labs(x = "", y = "Performance change with\nincreasing prediction horizon", colour = "") +
    theme_bw(base_size = 15) +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

  return(p2)

}

#' Plot performance (eg lpd_diff) vs score
#'
#' @param perf Dataframe of performance estimates
#' @param metric Name of the metric to plot
#' @param discrete Whether to plot as if y was a discrete outcome or a continuous one (loess is then applied)
#' @param max_score Maximum value that the score can take
#'
#' @return Ggplot
#'
#' @import ggplot2 dplyr
#' @export
plot_perf_vs_score <- function(perf, metric = "lpd_diff", discrete = FALSE, max_score = NA) {

  stopifnot(is.data.frame(perf),
            is_scalar_character(metric),
            all(c("Patient", "Time", "Score", "Model", metric) %in% colnames(perf)),
            is_scalar_logical(discrete),
            is_scalar(max_score),
            is.na(max_score) | is.numeric(max_score))

  mdl_names <- intersect(levels(perf[["Model"]]), unique(perf[["Model"]]))

  # Data
  preds <- perf %>%
    select(Patient, Time, Score) %>%
    drop_na() %>%
    distinct()

  perf <- rename(perf, Metric = all_of(metric))

  if (discrete) {

    # Compute cumulative proportion of y
    n_score <- preds %>%
      group_by(Score) %>%
      summarise(N = n()) %>%
      ungroup()
    if (nrow(n_score) != (max_score + 1)) {
      n_score <- bind_rows(n_score, data.frame(Score = setdiff(0:max_score, n_score$Score), N = 0))
    }
    n_score <- n_score %>%
      arrange(Score) %>%
      mutate(CumProp = cumsum(N) / sum(N))

    # Average on y
    tmp <- perf %>%
      group_by(Score, Model) %>%
      summarise(Mean = mean(Metric), SD = stats::sd(Metric), N = n(), SE = SD / sqrt(N))

    p4 <- ggplot(data = tmp,
                 aes(x = Score, y = Mean, ymin = Mean - SE, ymax = Mean + SE, colour = Model)) +
      geom_pointrange() +
      geom_line() +
      scale_x_continuous(limits = c(0, max_score),
                         breaks = 0:max_score,
                         sec.axis = dup_axis(breaks = n_score[["Score"]],
                                             labels = signif(n_score[["CumProp"]], 2),
                                             name = "Cumulative proportion of observations"))
  } else {
    p4 <- ggplot(data = perf,
                 aes(x = Score, y = Metric, colour = Model, fill = Model)) +
      geom_smooth(alpha = 0.5, method = "loess") +
      scale_x_continuous(limits = c(0, max_score),
                         sec.axis = dup_axis(breaks = stats::quantile(preds[["Score"]], probs = c(0, .1, .25, .5, .75, .9, .99)),
                                             labels = c(0, .1, .25, .5, .75, .9, .99),
                                             name = "Cumulative proportion of observations"))
  }
  p4 <- p4 +
    scale_colour_manual(values = rev(HuraultMisc::cbbPalette[1:length(mdl_names)]), guide = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = rev(HuraultMisc::cbbPalette[1:length(mdl_names)]), guide = guide_legend(reverse = TRUE)) +
    labs(y = metric, colour = "", fill = "") +
    theme_bw(base_size = 15) +
    theme(panel.grid.minor.y = element_blank(),
          legend.position = "top")

  return(p4)

}
