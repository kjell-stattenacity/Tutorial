
library(tidymodels)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

preproc <- c("No Pre-Processing", "1, 2, 15", "2, 2, 15", "1, 2, 49", "2, 2, 49")

# ------------------------------------------------------------------------------
# No pre-processing results

load("RData/preproc_results_0.RData")

all_metrics <- 
  cb_metrics_0 %>% 
  filter(.metric == "rmse") %>% 
  mutate(Model = "Cubist", Pre = "No Pre-Processing") %>% 
  select(Model, Pre, RMSE = mean, n, std_err, .config) %>% 
  bind_rows(
    rf_metrics_0 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Random Forest", Pre = "No Pre-Processing") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    pls_metrics_0 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Partial Least Squares", Pre = "No Pre-Processing") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>%   
  bind_rows(
    svm_metrics_0 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Support Vector Machine", Pre = "No Pre-Processing") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) 

all_pred <- 
  cb_pred_0 %>% 
  mutate(Model = "Cubist", Pre = "No Pre-Processing") %>% 
  select(Model, Pre, .pred, concentration, .config) %>% 
  bind_rows(
    rf_pred_0 %>% 
      mutate(Model = "Random Forest", Pre = "No Pre-Processing") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    pls_pred_0 %>% 
      mutate(Model = "Partial Least Squares", Pre = "No Pre-Processing") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>%   
  bind_rows(
    svm_pred_0 %>% 
      mutate(Model = "Support Vector Machine", Pre = "No Pre-Processing") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) 

# ------------------------------------------------------------------------------
# Analysis of pre-processor 1: diffs = 1, poly = 2, window = 15

load("RData/preproc_results_1.RData")

all_metrics <- 
  all_metrics %>% 
  bind_rows(
    cb_metrics_1 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Cubist", Pre = "1, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    rf_metrics_1 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Random Forest", Pre = "1, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    pls_metrics_1 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Partial Least Squares", Pre = "1, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>%   
  bind_rows(
    svm_metrics_1 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Support Vector Machine", Pre = "1, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) 

all_pred <- 
  all_pred %>% 
  bind_rows(
    cb_pred_1 %>% 
      mutate(Model = "Cubist", Pre = "1, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    rf_pred_1 %>% 
      mutate(Model = "Random Forest", Pre = "1, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    pls_pred_1 %>% 
      mutate(Model = "Partial Least Squares", Pre = "1, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>%   
  bind_rows(
    svm_pred_1 %>% 
      mutate(Model = "Support Vector Machine", Pre = "1, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  )

# ------------------------------------------------------------------------------
# Analysis of pre-processor 2: diffs = 2, poly = 2, window = 15

load("RData/preproc_results_2.RData")

all_metrics <- 
  all_metrics %>% 
  bind_rows(
    cb_metrics_2 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Cubist", Pre = "2, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    rf_metrics_2 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Random Forest", Pre = "2, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    pls_metrics_2 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Partial Least Squares", Pre = "2, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>%   
  bind_rows(
    svm_metrics_2 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Support Vector Machine", Pre = "2, 2, 15") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) 

all_pred <- 
  all_pred %>% 
  bind_rows(
    cb_pred_2 %>% 
      mutate(Model = "Cubist", Pre = "2, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    rf_pred_2 %>% 
      mutate(Model = "Random Forest", Pre = "2, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    pls_pred_2 %>% 
      mutate(Model = "Partial Least Squares", Pre = "2, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>%   
  bind_rows(
    svm_pred_2 %>% 
      mutate(Model = "Support Vector Machine", Pre = "2, 2, 15") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  )

# ------------------------------------------------------------------------------
# Analysis of pre-processor 3: diffs = 1, poly = 2, window = 49

load("RData/preproc_results_3.RData")

all_metrics <- 
  all_metrics %>% 
  bind_rows(
    cb_metrics_3 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Cubist", Pre = "1, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    rf_metrics_3 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Random Forest", Pre = "1, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    pls_metrics_3 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Partial Least Squares", Pre = "1, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>%   
  bind_rows(
    svm_metrics_3 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Support Vector Machine", Pre = "1, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) 

all_pred <- 
  all_pred %>% 
  bind_rows(
    cb_pred_2 %>% 
      mutate(Model = "Cubist", Pre = "1, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    rf_pred_2 %>% 
      mutate(Model = "Random Forest", Pre = "1, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    pls_pred_2 %>% 
      mutate(Model = "Partial Least Squares", Pre = "1, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>%   
  bind_rows(
    svm_pred_2 %>% 
      mutate(Model = "Support Vector Machine", Pre = "1, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  )

# ------------------------------------------------------------------------------
# Analysis of pre-processor 4: diffs = 1, poly = 2, window = 49

load("RData/preproc_results_4.RData")

all_metrics <- 
  all_metrics %>% 
  bind_rows(
    cb_metrics_4 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Cubist", Pre = "2, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    rf_metrics_4 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Random Forest", Pre = "2, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>% 
  bind_rows(
    pls_metrics_4 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Partial Least Squares", Pre = "2, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) %>%   
  bind_rows(
    svm_metrics_4 %>% 
      filter(.metric == "rmse") %>% 
      mutate(Model = "Support Vector Machine", Pre = "2, 2, 49") %>% 
      select(Model, Pre, RMSE = mean, n, std_err, .config)
  ) 

all_pred <- 
  all_pred %>% 
  bind_rows(
    cb_pred_2 %>% 
      mutate(Model = "Cubist", Pre = "2, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    rf_pred_2 %>% 
      mutate(Model = "Random Forest", Pre = "2, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>% 
  bind_rows(
    pls_pred_2 %>% 
      mutate(Model = "Partial Least Squares", Pre = "2, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  ) %>%   
  bind_rows(
    svm_pred_2 %>% 
      mutate(Model = "Support Vector Machine", Pre = "2, 2, 49") %>% 
      select(Model, Pre, .pred, concentration, .config) 
  )

# ------------------------------------------------------------------------------
# best results per model

all_best <- 
  all_metrics %>% 
  slice_min(RMSE, by = c(Model, Pre)) %>% 
  mutate(Pre = factor(Pre, levels = preproc))

all_pred <- 
  all_pred %>% 
  inner_join(all_best, by = c("Model", "Pre", ".config")) %>% 
  mutate(Pre = factor(Pre, levels = preproc))
  

all_pred %>% 
  ggplot(aes(concentration, .pred)) + 
  geom_abline(col = "green", alpha = 1 / 2) + 
  geom_point(alpha = 1 / 2) + 
  facet_grid(Model ~ Pre) + 
  coord_obs_pred()

all_best %>% 
  ggplot(aes(x = Pre, y = RMSE, group = Model, col = Model, pch = Model)) + 
  geom_line() + 
  geom_point(cex = 2)

# ------------------------------------------------------------------------------

all_pls_rmse <- 
  bind_rows(
    pls_metrics_0 %>% mutate(Pre = "No Pre-Processing"),
    pls_metrics_1 %>% mutate(Pre = "1, 2, 15"),
    pls_metrics_2 %>% mutate(Pre = "2, 2, 15"),
    pls_metrics_3 %>% mutate(Pre = "1, 2, 49"),
    pls_metrics_4 %>% mutate(Pre = "2, 2, 49")
  ) %>% 
  filter(.metric == "rmse") %>% 
  mutate(Pre = factor(Pre, levels = preproc))


all_rf_rmse <- 
  bind_rows(
    rf_metrics_0 %>% mutate(Pre = "No Pre-Processing"),
    rf_metrics_1 %>% mutate(Pre = "1, 2, 15"),
    rf_metrics_2 %>% mutate(Pre = "2, 2, 15"),
    rf_metrics_3 %>% mutate(Pre = "1, 2, 49"),
    rf_metrics_4 %>% mutate(Pre = "2, 2, 49")
  ) %>% 
  filter(.metric == "rmse") %>% 
  mutate(Pre = factor(Pre, levels = preproc))


# ------------------------------------------------------------------------------
# Collate results for this pre-processing configuration

res <- ls(pattern = "^all_")
save(list = res, file = "RData/all_results.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
