
library(tidymodels)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------
# helpers

models <- c( "Partial Least Squares", "Random Forest", "Cubist", "Support Vector Machine")
preproc <- c("No Pre-Processing", "(1, 2, 15)", "(2, 2, 15)", "(1, 2, 49)", "(2, 2, 49)")

make_preproc_code <- function(do, po, ws) {
  ifelse( 
    is.na(do), 
    "No Pre-Processing",
    paste0("(", do, ", ", po, ", ", ws, ")")
  )
}

# ------------------------------------------------------------------------------
# load results

load("RData/preproc_results_0.RData")
load("RData/preproc_results_1.RData")
load("RData/preproc_results_2.RData")
load("RData/preproc_results_3.RData")
load("RData/preproc_results_4.RData")

pull_results <- function(pattern, label) {
  objs <- ls(envir = rlang::global_env(), pattern = pattern)
  rlang::inject(bind_rows(!!!rlang::syms(objs))) %>% 
    mutate(
      Model = label,
      Model = factor(Model, levels = models),
      Pre = make_preproc_code(differentiation_order, polynomial_order, window_size),
      Pre = factor(Pre, levels = preproc),
    )
}

# ------------------------------------------------------------------------------
# Assemble all results

all_pls_metrics <- pull_results("pls_metrics",  "Partial Least Squares")
all_rf_metrics  <- pull_results("rf_metrics",   "Random Forest")
all_cb_metrics  <- pull_results("cb_metrics",   "Cubist")
all_svm_metrics <- pull_results("svm_metrics", "Support Vector Machine")

all_pls_preds <- pull_results("pls_pred",  "Partial Least Squares")
all_rf_preds  <- pull_results("rf_pred",   "Random Forest")
all_cb_preds  <- pull_results("cb_pred",   "Cubist")
all_svm_preds <- pull_results("svm_pred",  "Support Vector Machine")


keep_cols <- c("Model", "Pre", ".metric", "mean", "n", "std_err", ".config", 
               "differentiation_order", "polynomial_order", "window_size")
all_rmse <- 
  bind_rows(
    all_pls_metrics %>% select(all_of(keep_cols)), 
    all_rf_metrics %>% select(all_of(keep_cols)), 
    all_cb_metrics %>% select(all_of(keep_cols)), 
    all_svm_metrics %>% select(all_of(keep_cols))
  ) %>% 
  filter(.metric == "rmse") %>% 
  rename(RMSE = mean) %>% 
  select(-.metric) %>% 
  mutate(
    Model = factor(Model, levels = models),
    Pre = factor(Pre, levels = preproc)
  )


# ------------------------------------------------------------------------------
# best results per model

all_best <- 
  all_rmse %>% 
  slice_min(
    RMSE, 
    by = c(Model, Pre, differentiation_order, polynomial_order, window_size),
    with_ties = FALSE
  )


keep_cols <- c("Model", "Pre", ".row", "concentration", ".pred", ".config", 
               "differentiation_order", "polynomial_order", "window_size")
all_pred <- 
  bind_rows(
    all_pls_preds %>% select(all_of(keep_cols)), 
    all_rf_preds %>% select(all_of(keep_cols)), 
    all_cb_preds %>% select(all_of(keep_cols)), 
    all_svm_preds %>% select(all_of(keep_cols))
  ) %>% 
  inner_join(
    all_best %>% select(-RMSE, -n, -std_err), 
    by = join_by(Model, Pre, .config, differentiation_order, polynomial_order, window_size)
  ) %>% 
  mutate(
    Model = factor(Model, levels = models),
    Pre = factor(Pre, levels = preproc)
  )

if (interactive()) {
  all_pred %>% 
    ggplot(aes(concentration, .pred)) + 
    geom_abline(col = "green", alpha = 1 / 2) + 
    geom_point(alpha = 1 / 2) + 
    facet_grid(Model ~ Pre) + 
    coord_obs_pred()
}
  

# ------------------------------------------------------------------------------
# Collate PCA results

load("RData/pca_results_0.RData")
load("RData/pca_results_1.RData")
load("RData/pca_results_2.RData")
load("RData/pca_results_3.RData")
load("RData/pca_results_4.RData")

pca_data_objs <- ls(pattern = "pca_data_")
all_pca_data <- 
  rlang::inject(bind_rows(!!!rlang::syms(pca_data_objs))) %>% 
  mutate(
    Pre = make_preproc_code(differentiation_order, polynomial_order, window_size),
    Pre = factor(Pre, levels = preproc)
  )

pca_var_objs <- ls(pattern = "pca_var_")
all_pca_var <- 
  rlang::inject(bind_rows(!!!rlang::syms(pca_var_objs))) %>% 
  mutate(
    Pre = make_preproc_code(differentiation_order, polynomial_order, window_size),
    Pre = factor(Pre, levels = preproc)
  )


# ------------------------------------------------------------------------------
# Save results

res <- ls(pattern = "^all_")
save(list = res, file = "RData/all_results.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
