# The analysis_#.R files have roughly the same format. To see code comments, 
# take a look at analysis_1.R
library(tidymodels)
library(plsmod)
library(rules)
library(sfd) # topepo/sfd
library(doMC)

# ------------------------------------------------------------------------------
# Set some options

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)
registerDoMC(cores = parallel::detectCores())

# ------------------------------------------------------------------------------
# Load all processed data configurations

load("RData/processed_data.RData")
load("RData/data_wide.RData")

# ------------------------------------------------------------------------------
# Analysis of pre-processor 2: diffs = 2, poly = 2, window = 15

preproc_2 <- tibble(differentiation_order = 2, polynomial_order = 2, window_size = 15)

data_2 <- 
  processed_data %>% 
  inner_join(preproc_2, by = c("differentiation_order", "polynomial_order", "window_size")) %>% 
  dplyr::select(-differentiation_order, -polynomial_order, -window_size)

set.seed(910)
split_2 <- initial_split(data_2, strata = concentration, prop = 0.77)
train_2 <- training(split_2)
test_2  <- testing(split_2)

set.seed(522)
folds_2 <- vfold_cv(train_2, v = 10, repeats = 5)

base_rec_2 <- 
  recipe(concentration ~ ., data = train_2) %>% 
  # For some larger values of window_size, there are columns with all missing
  # predictor values; let's remove those by determining that they have a single
  # unique value
  step_zv(all_predictors()) %>% 
  # We'll keep the sample ID in the data but not treat it as a predictor
  update_role(sample_id, new_role = "sample_id")

# ------------------------------------------------------------------------------
# Setup some options

grid_ctrl <- 
  control_grid(save_pred = TRUE,  # Save the hold-out predictions
               parallel_over = "everything")

bayes_ctrl <- control_bayes(save_pred = TRUE, no_improve = Inf)

# ------------------------------------------------------------------------------
# Partial least squares analysis

norm_rec_2 <- 
  base_rec_2 %>% 
  step_normalize(starts_with("x"))

pls_spec <- pls(num_comp = tune()) %>% set_mode("regression")

pls_wflow_2 <- 
  workflow() %>% 
  add_recipe(norm_rec_2) %>% 
  add_model(pls_spec)

pls_tune_2 <-
  tune_grid(pls_wflow_2,
            resamples = folds_2, 
            grid = tibble(num_comp = 1:25),
            control = grid_ctrl)

pls_metrics_2 <- 
  collect_metrics(pls_tune_2) %>% 
  cbind(preproc_2) %>% 
  as_tibble()

pls_pred_2 <- 
  collect_predictions(pls_tune_2, summarize = TRUE) %>% 
  cbind(preproc_2) %>% 
  as_tibble() %>% 
  inner_join(
    train_2 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
    by = ".row"
  ) %>% 
  dplyr::select(-.row)

# ------------------------------------------------------------------------------
# Random forest analysis

rf_spec <- 
  rand_forest(mtry = tune(), trees = 1000) %>%
  set_mode("regression")

prepped_2 <- prep(base_rec_2) %>% bake(new_data = NULL)
num_predictors_2 <- sum(grepl("^x", names(prepped_2)))
mtry_obj_2 <- mtry(c(2, num_predictors_2))
mtry_vals_2 <- unique(value_seq(mtry_obj_2, 25))
mtry_prop_2 <- tibble(mtry = mtry_vals_2, prop = mtry_vals_2 / num_predictors_2)

set.seed(382)
rf_tune_2 <-
  rf_spec %>% 
  tune_grid(base_rec_2,
            resamples = folds_2, 
            grid = tibble(mtry = mtry_vals_2),
            control = grid_ctrl)

rf_metrics_2 <- 
  collect_metrics(rf_tune_2) %>% 
  cbind(preproc_2) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_2, by = "mtry")

rf_pred_2 <- 
  collect_predictions(rf_tune_2, summarize = TRUE) %>% 
  cbind(preproc_2) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_2, by = "mtry") %>% 
  inner_join(
    train_2 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
    by = ".row"
  ) %>% 
  dplyr::select(-.row)


# ------------------------------------------------------------------------------
# Cubist analysis

cubist_spec <- cubist_rules(committees = tune(), neighbors = tune())

cb_grid <- 
  get_design(2, 25) %>% 
  update_values(
    list(committees() %>% value_seq(25), rep_len(0:9, 25))
  ) %>% 
  setNames(c("committees", "neighbors"))


set.seed(382)
cb_tune_2 <-
  cubist_spec %>% 
  tune_grid(base_rec_2,
            resamples = folds_2, 
            grid = cb_grid,
            control = grid_ctrl)

cb_final_2 <- select_best(cb_tune_2, metric = "rmse")

cb_metrics_2 <- 
  collect_metrics(cb_tune_2) %>% 
  cbind(preproc_2) %>% 
  as_tibble() 

cb_pred_2 <- 
  collect_predictions(cb_tune_2, summarize = TRUE) %>% 
  cbind(preproc_2) %>% 
  as_tibble() %>% 
  inner_join(
    train_2 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
    by = ".row"
  ) %>% 
  dplyr::select(-.row) 


# ------------------------------------------------------------------------------
# Support vector machine analysis

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_mode("regression")

svm_wflow_2 <- 
  workflow() %>% 
  add_recipe(norm_rec_2) %>% 
  add_model(svm_spec)
  
set.seed(382)
svm_init_2 <-
  svm_spec %>% 
  tune_grid(base_rec_2,
            resamples = folds_2, 
            grid = 10,
            control = grid_ctrl)

set.seed(382)
svm_tune_2 <-
  svm_spec %>% 
  tune_bayes(base_rec_2,
             resamples = folds_2, 
             iter = 15,
             initial = svm_init_2,
             control = bayes_ctrl)

svm_final_2 <- select_best(svm_tune_2, metric = "rmse")

svm_metrics_2 <- 
  collect_metrics(svm_tune_2) %>% 
  cbind(preproc_2) %>% 
  as_tibble() 

svm_pred_2 <- 
  collect_predictions(svm_tune_2, summarize = TRUE) %>% 
  cbind(preproc_2) %>% 
  as_tibble() %>% 
  inner_join(
    train_2 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
    by = ".row"
  ) %>% 
  dplyr::select(-.row) 

# ------------------------------------------------------------------------------
# PCA components for diagnostic plots 

rec_pca_2 <- 
  norm_rec_2 %>% 
  step_normalize(starts_with("x")) %>% 
  step_pca(starts_with("x"), num_comp = 5, id = "pca") %>% 
  prep()

pca_data_2 <- 
  rec_pca_2 %>% 
  bake(new_data = NULL) %>% 
  cbind(preproc_2) %>% 
  as_tibble() 

pca_var_2 <- 
  rec_pca_2 %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "cumulative percent variance") %>% 
  dplyr::select(value, component) %>% 
  cbind(preproc_2) %>% 
  as_tibble() 

# ------------------------------------------------------------------------------
# Collate results for this pre-processing configuration

res_2 <- ls(pattern = "(_metrics_2)|(_pred_2)|(_final_2)")
save(list = res_2, file = "RData/preproc_results_2.RData", compress = TRUE)

res_pca_2 <- ls(pattern = "^pca_")
save(list = res_pca_2, file = "RData/pca_results_2.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}

