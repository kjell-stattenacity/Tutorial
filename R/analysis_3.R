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

load("RData/processed_data.RData")
load("RData/data_wide.RData")

# ------------------------------------------------------------------------------
# Analysis of pre-processor 3: diffs = 1, poly = 2, window = 49

preproc_3 <- tibble(differentiation_order = 1, polynomial_order = 2, window_size = 49)

data_3 <- 
  processed_data %>% 
  inner_join(preproc_3, by = c("differentiation_order", "polynomial_order", "window_size")) %>% 
  select(-differentiation_order, -polynomial_order, -window_size)

set.seed(910)
split_3 <- initial_split(data_3, strata = concentration, prop = 0.77)
train_3 <- training(split_3)
test_3  <- testing(split_3)

set.seed(522)
folds_3 <- vfold_cv(train_3, v = 10, repeats = 10)

base_rec_3 <- 
  recipe(concentration ~ ., data = train_3) %>% 
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

norm_rec_3 <- 
  base_rec_3 %>% 
  step_normalize(starts_with("x"))

pls_spec <- pls(num_comp = tune()) %>% set_mode("regression")

pls_wflow_3 <- 
  workflow() %>% 
  add_recipe(norm_rec_3) %>% 
  add_model(pls_spec)

pls_tune_3 <-
  tune_grid(pls_wflow_3,
            resamples = folds_3, 
            grid = tibble(num_comp = 1:25),
            control = grid_ctrl)

pls_metrics_3 <- 
  collect_metrics(pls_tune_3) %>% 
  cbind(preproc_3) %>% 
  as_tibble()

pls_pred_3 <- 
  collect_predictions(pls_tune_3, summarize = TRUE) %>% 
  cbind(preproc_3) %>% 
  as_tibble()

# ------------------------------------------------------------------------------
# Random forest analysis

rf_spec <- 
  rand_forest(mtry = tune(), trees = 1000) %>%
  set_mode("regression")

num_predictors_3 <- sum(grepl("^x", names(train_3)))
mtry_obj_3 <- mtry(c(2, num_predictors_3))
mtry_vals_3 <- unique(value_seq(mtry_obj_3, 25))
mtry_prop_3 <- tibble(mtry = mtry_vals_3, prop = mtry_vals_3 / num_predictors_3)

set.seed(382)
rf_tune_3 <-
  rf_spec %>% 
  tune_grid(base_rec_3,
            resamples = folds_3, 
            grid = tibble(mtry = mtry_vals_3),
            control = grid_ctrl)

rf_metrics_3 <- 
  collect_metrics(rf_tune_3) %>% 
  cbind(preproc_3) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_3, by = "mtry")

rf_pred_3 <- 
  collect_predictions(rf_tune_3, summarize = TRUE) %>% 
  cbind(preproc_3) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_3, by = "mtry")


# ------------------------------------------------------------------------------
# Cubist analysis

cubist_spec <- cubist_rules(committees = tune(), neighbors = tune())

set.seed(382)
cb_tune_3 <-
  cubist_spec %>% 
  tune_grid(base_rec_3,
            resamples = folds_3, 
            grid = 25,
            control = grid_ctrl)

cb_metrics_3 <- 
  collect_metrics(cb_tune_3) %>% 
  cbind(preproc_3) %>% 
  as_tibble() 

cb_pred_3 <- 
  collect_predictions(cb_tune_3, summarize = TRUE) %>% 
  cbind(preproc_3) %>% 
  as_tibble() 


# ------------------------------------------------------------------------------
# Support vector machine analysis

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_mode("regression")

svm_wflow_3 <- 
  workflow() %>% 
  add_recipe(norm_rec_3) %>% 
  add_model(svm_spec)
  
set.seed(382)
svm_init_3 <-
  svm_spec %>% 
  tune_grid(base_rec_3,
            resamples = folds_3, 
            grid = 10,
            control = grid_ctrl)

set.seed(382)
svm_tune_3 <-
  svm_spec %>% 
  tune_bayes(base_rec_3,
             resamples = folds_3, 
             iter = 15,
             initial = svm_init_3,
             control = bayes_ctrl)

svm_metrics_3 <- 
  collect_metrics(svm_tune_3) %>% 
  cbind(preproc_3) %>% 
  as_tibble() 

svm_pred_3 <- 
  collect_predictions(svm_tune_3, summarize = TRUE) %>% 
  cbind(preproc_3) %>% 
  as_tibble() 

# ------------------------------------------------------------------------------
# Collate results for this pre-processing configuration

res_3 <- ls(pattern = "(_metrics_3)|(_pred_3)")
save(list = res_3, file = "RData/preproc_results_3.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}

