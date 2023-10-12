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
# Analysis of pre-processor 4: diffs = 2, poly = 2, window = 49

preproc_4 <- tibble(differentiation_order = 2, polynomial_order = 2, window_size = 49)

data_4 <- 
  processed_data %>% 
  inner_join(preproc_4, by = c("differentiation_order", "polynomial_order", "window_size")) %>% 
  dplyr::select(-differentiation_order, -polynomial_order, -window_size)

set.seed(910)
split_4 <- initial_split(data_4, strata = concentration, prop = 0.77)
train_4 <- training(split_4)
test_4  <- testing(split_4)

set.seed(522)
folds_4 <- vfold_cv(train_4, v = 10, repeats = 5)

base_rec_4 <- 
  recipe(concentration ~ ., data = train_4) %>% 
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

norm_rec_4 <- 
  base_rec_4 %>% 
  step_normalize(starts_with("x"))

pls_spec <- pls(num_comp = tune()) %>% set_mode("regression")

pls_wflow_4 <- 
  workflow() %>% 
  add_recipe(norm_rec_4) %>% 
  add_model(pls_spec)

pls_tune_4 <-
  tune_grid(pls_wflow_4,
            resamples = folds_4, 
            grid = tibble(num_comp = 1:25),
            control = grid_ctrl)

pls_metrics_4 <- 
  collect_metrics(pls_tune_4) %>% 
  cbind(preproc_4) %>% 
  as_tibble()

pls_pred_4 <- 
  collect_predictions(pls_tune_4, summarize = TRUE) %>% 
  cbind(preproc_4) %>% 
  as_tibble() %>% 
  inner_join(
    train_4 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
    by = ".row"
  ) %>% 
  dplyr::select(-.row)

# ------------------------------------------------------------------------------
# Random forest analysis

rf_spec <- 
  rand_forest(mtry = tune(), trees = 1000) %>%
  set_mode("regression")

prepped_4 <- prep(base_rec_4) %>% bake(new_data = NULL)
num_predictors_4 <- sum(grepl("^x", names(prepped_4)))
mtry_obj_4 <- mtry(c(2, num_predictors_4))
mtry_vals_4 <- unique(value_seq(mtry_obj_4, 25))
mtry_prop_4 <- tibble(mtry = mtry_vals_4, prop = mtry_vals_4 / num_predictors_4)

set.seed(382)
rf_tune_4 <-
  rf_spec %>% 
  tune_grid(base_rec_4,
            resamples = folds_4, 
            grid = tibble(mtry = mtry_vals_4),
            control = grid_ctrl)

rf_metrics_4 <- 
  collect_metrics(rf_tune_4) %>% 
  cbind(preproc_4) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_4, by = "mtry")

rf_pred_4 <- 
  collect_predictions(rf_tune_4, summarize = TRUE) %>% 
  cbind(preproc_4) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_4, by = "mtry") %>% 
  inner_join(
    train_4 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
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
cb_tune_4 <-
  cubist_spec %>% 
  tune_grid(base_rec_4,
            resamples = folds_4, 
            grid = cb_grid,
            control = grid_ctrl)

cb_final_4 <- select_best(cb_tune_4, metric = "rmse")

cb_metrics_4 <- 
  collect_metrics(cb_tune_4) %>% 
  cbind(preproc_4) %>% 
  as_tibble() 

cb_pred_4 <- 
  collect_predictions(cb_tune_4, summarize = TRUE) %>% 
  cbind(preproc_4) %>% 
  as_tibble() %>% 
  inner_join(
    train_4 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
    by = ".row"
  ) %>% 
  dplyr::select(-.row) 


# ------------------------------------------------------------------------------
# Support vector machine analysis

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_mode("regression")

svm_wflow_4 <- 
  workflow() %>% 
  add_recipe(norm_rec_4) %>% 
  add_model(svm_spec)
  
set.seed(382)
svm_init_4 <-
  svm_spec %>% 
  tune_grid(base_rec_4,
            resamples = folds_4, 
            grid = 10,
            control = grid_ctrl)

set.seed(382)
svm_tune_4 <-
  svm_spec %>% 
  tune_bayes(base_rec_4,
             resamples = folds_4, 
             iter = 15,
             initial = svm_init_4,
             control = bayes_ctrl)

svm_final_4 <- select_best(svm_tune_4, metric = "rmse")

svm_metrics_4 <- 
  collect_metrics(svm_tune_4) %>% 
  cbind(preproc_4) %>% 
  as_tibble() 

svm_pred_4 <- 
  collect_predictions(svm_tune_4, summarize = TRUE) %>% 
  cbind(preproc_4) %>% 
  as_tibble() %>% 
  inner_join(
    train_4 %>% add_rowindex() %>% dplyr::select(.row, sample_id),
    by = ".row"
  ) %>% 
  dplyr::select(-.row) 

# ------------------------------------------------------------------------------
# PCA components for diagnostic plots 

rec_pca_4 <- 
  norm_rec_4 %>% 
  step_normalize(starts_with("x")) %>% 
  step_pca(starts_with("x"), num_comp = 5, id = "pca") %>% 
  prep()

pca_data_4 <- 
  rec_pca_4 %>% 
  bake(new_data = NULL) %>% 
  cbind(preproc_4) %>% 
  as_tibble() 

pca_var_4 <- 
  rec_pca_4 %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "cumulative percent variance") %>% 
  dplyr::select(value, component) %>% 
  cbind(preproc_4) %>% 
  as_tibble() 

# ------------------------------------------------------------------------------
# Collate results for this pre-processing configuration

res_4 <- ls(pattern = "(_metrics_4)|(_pred_4)|(_final_4)")
save(list = res_4, file = "RData/preproc_results_4.RData", compress = TRUE)

res_pca_4 <- ls(pattern = "^pca_")
save(list = res_pca_4, file = "RData/pca_results_4.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}

