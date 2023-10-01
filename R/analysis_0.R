# The analysis_#.R files have roughly the same format. To see code comments, 
# take a look at analysis_1.R
library(tidymodels)
library(plsmod)
library(rules)
library(doMC)

# ------------------------------------------------------------------------------
# Set some options

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)
registerDoMC(cores = parallel::detectCores())

# ------------------------------------------------------------------------------
# Load all processed data configurations

load("RData/data_wide.RData")

# ------------------------------------------------------------------------------
# No pre-processing

set.seed(910)
split_0 <- initial_split(data_wide, strata = concentration, prop = 0.77)
train_0 <- training(split_0)
test_0  <- testing(split_0)

set.seed(522)
folds_0 <- vfold_cv(train_0, v = 10, repeats = 5)

base_rec_0 <- 
  recipe(concentration ~ ., data = train_0) %>% 
  update_role(sample_id, new_role = "sample_id")

# ------------------------------------------------------------------------------
# Setup some options

grid_ctrl <- 
  control_grid(save_pred = TRUE,  # Save the hold-out predictions
               parallel_over = "everything")

bayes_ctrl <- control_bayes(save_pred = TRUE)

# ------------------------------------------------------------------------------
# Partial least squares analysis

norm_rec_0 <- 
  base_rec_0 %>% 
  step_normalize(starts_with("x"))

pls_spec <- pls(num_comp = tune()) %>% set_mode("regression")

pls_wflow_0 <- 
  workflow() %>% 
  add_recipe(norm_rec_0) %>% 
  add_model(pls_spec)

pls_tune_0 <-
  tune_grid(pls_wflow_0,
            resamples = folds_0, 
            grid = tibble(num_comp = 1:15),
            control = grid_ctrl)

pls_metrics_0 <- 
  collect_metrics(pls_tune_0) 

pls_pred_0 <- 
  collect_predictions(pls_tune_0, summarize = TRUE) 

# ------------------------------------------------------------------------------
# Random forest analysis

rf_spec <- 
  rand_forest(mtry = tune(), trees = 1000) %>%
  set_mode("regression")

num_predictors_0 <- sum(grepl("^x", names(train_0)))
mtry_obj_0 <- mtry(c(2, num_predictors_0))
mtry_vals_0 <- value_seq(mtry_obj_0, 15)
mtry_prop_0 <- tibble(mtry = mtry_vals_0, prop = mtry_vals_0 / num_predictors_0)

set.seed(382)
rf_tune_0 <-
  rf_spec %>% 
  tune_grid(base_rec_0,
            resamples = folds_0, 
            grid = tibble(mtry = mtry_vals_0),
            control = grid_ctrl)

rf_metrics_0 <- 
  collect_metrics(rf_tune_0) %>% 
  full_join(mtry_prop_0, by = "mtry")

rf_pred_0 <- 
  collect_predictions(rf_tune_0, summarize = TRUE) %>% 
  full_join(mtry_prop_0, by = "mtry")


# ------------------------------------------------------------------------------
# Cubist analysis

cubist_spec <- cubist_rules(committees = tune(), neighbors = tune())

set.seed(382)
cb_tune_0 <-
  cubist_spec %>% 
  tune_grid(base_rec_0,
            resamples = folds_0, 
            grid = 15,
            control = grid_ctrl)

cb_metrics_0 <- 
  collect_metrics(cb_tune_0) 

cb_pred_0 <- 
  collect_predictions(cb_tune_0, summarize = TRUE) 


# ------------------------------------------------------------------------------
# Support vector machine analysis

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_mode("regression")

svm_wflow_0 <- 
  workflow() %>% 
  add_recipe(norm_rec_0) %>% 
  add_model(svm_spec)
  
set.seed(382)
svm_init_0 <-
  svm_spec %>% 
  tune_grid(base_rec_0,
            resamples = folds_0, 
            grid = 5,
            control = grid_ctrl)

set.seed(382)
svm_tune_0 <-
  svm_spec %>% 
  tune_bayes(base_rec_0,
             resamples = folds_0, 
             iter = 10,
             initial = svm_init_0,
             control = bayes_ctrl)

svm_metrics_0 <- 
  collect_metrics(svm_tune_0)

svm_pred_0 <- 
  collect_predictions(svm_tune_0, summarize = TRUE) 

# ------------------------------------------------------------------------------
# Collate results for this pre-processing configuration

res_0 <- ls(pattern = "(_metrics_0)|(_pred_0)")
save(list = res_0, file = "RData/preproc_results_0.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}

