library(tidymodels)
library(plsmod)
library(rules)

# We'll use an optimized space-filling deisng not yet in tidymodels
library(sfd) # topepo/sfd

# We use parallel processing on unix via forking. This pacakge is not available 
# for windows but you can do something similar with the doParallel package
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
# Analysis of pre-processor 1: diffs = 1, poly = 2, window = 15

preproc_1 <- tibble(differentiation_order = 1, polynomial_order = 2, window_size = 15)

data_1 <- 
  processed_data %>% 
  inner_join(preproc_1, by = c("differentiation_order", "polynomial_order", "window_size")) %>% 
  select(-differentiation_order, -polynomial_order, -window_size)

set.seed(910)
split_1 <- initial_split(data_1, strata = concentration, prop = 0.77)
train_1 <- training(split_1)
test_1  <- testing(split_1)

set.seed(522)
folds_1 <- vfold_cv(train_1, v = 10, repeats = 5)

base_rec_1 <- 
  recipe(concentration ~ ., data = train_1) %>% 
  # For some larger values of window_size, there are columns with all missing
  # predictor values; let's remove those by determining that they have a single
  # unique value
  step_zv(all_predictors()) %>% 
  # We'll keep the sample ID in the data but not treat it as a predictor
  update_role(sample_id, new_role = "sample_id")

# ------------------------------------------------------------------------------
# Setup some options

# Save the hold-out predictions
grid_ctrl <- control_grid(save_pred = TRUE, parallel_over = "everything")
bayes_ctrl <- control_bayes(save_pred = TRUE, no_improve = Inf)

# ------------------------------------------------------------------------------
# Partial least squares analysis

# Add centering and scaling befor PLS
norm_rec_1 <- 
  base_rec_1 %>% 
  step_normalize(starts_with("x"))

pls_spec <- pls(num_comp = tune()) %>% set_mode("regression")

pls_wflow_1 <- 
  workflow() %>% 
  add_recipe(norm_rec_1) %>% 
  add_model(pls_spec)

pls_tune_1 <-
  tune_grid(pls_wflow_1,
            resamples = folds_1, 
            grid = tibble(num_comp = 1:25),
            control = grid_ctrl)

pls_metrics_1 <- 
  collect_metrics(pls_tune_1) %>% 
  cbind(preproc_1) %>% 
  as_tibble()

pls_pred_1 <- 
  collect_predictions(pls_tune_1, summarize = TRUE) %>% 
  cbind(preproc_1) %>% 
  as_tibble()

# ------------------------------------------------------------------------------
# Random forest analysis

rf_spec <- 
  rand_forest(mtry = tune(), trees = 1000) %>%
  set_mode("regression")

# For random forest, the range of mtry depends on the number of columns and that
# number will change over different preprocessing values. Here we figure out the
# number of predictors, make a grid, and also convert mtry to a proportion that
# can be used for plotting later. 
# However, larger differentiation orders result in columns with all missing values.
# For this reason, we prep the recipe on the training set and derive the number 
# of predictors from the process version of the training set. 

prepped_1 <- prep(base_rec_1) %>% bake(new_data = NULL)
num_predictors_1 <- sum(grepl("^x", names(prepped_1)))
mtry_obj_1 <- mtry(c(1, num_predictors_1))
mtry_vals_1 <- unique(value_seq(mtry_obj_1, 25))
mtry_prop_1 <- tibble(mtry = mtry_vals_1, prop = mtry_vals_1 / num_predictors_1)

set.seed(382)
rf_tune_1 <-
  rf_spec %>% 
  tune_grid(base_rec_1,
            resamples = folds_1, 
            grid = tibble(mtry = mtry_vals_1),
            control = grid_ctrl)

rf_metrics_1 <- 
  collect_metrics(rf_tune_1) %>% 
  cbind(preproc_1) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_1, by = "mtry")

rf_pred_1 <- 
  collect_predictions(rf_tune_1, summarize = TRUE) %>% 
  cbind(preproc_1) %>% 
  as_tibble() %>% 
  full_join(mtry_prop_1, by = "mtry")


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
cb_tune_1 <-
  cubist_spec %>% 
  tune_grid(base_rec_1,
            resamples = folds_1, 
            grid = cb_grid,
            control = grid_ctrl)

cb_metrics_1 <- 
  collect_metrics(cb_tune_1) %>% 
  cbind(preproc_1) %>% 
  as_tibble() 

cb_pred_1 <- 
  collect_predictions(cb_tune_1, summarize = TRUE) %>% 
  cbind(preproc_1) %>% 
  as_tibble() 


# ------------------------------------------------------------------------------
# Support vector machine analysis

svm_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_mode("regression")

svm_wflow_1 <- 
  workflow() %>% 
  add_recipe(norm_rec_1) %>% 
  add_model(svm_spec)
  
# In the tuning parameter space, SVMs tend to have flat plains with a small
# 'hill' of good performance. We'll start with a small space-filling design and
# then use Bayesian optimization to try to iteratively search for the hill. 
# Otherwise we might need a much larger grid. 
set.seed(382)
svm_init_1 <-
  svm_spec %>% 
  tune_grid(base_rec_1,
            resamples = folds_1, 
            grid = 10,
            control = grid_ctrl)

set.seed(382)
svm_tune_1 <-
  svm_spec %>% 
  tune_bayes(base_rec_1,
             resamples = folds_1, 
             iter = 15,
             initial = svm_init_1,
             control = bayes_ctrl)

svm_metrics_1 <- 
  collect_metrics(svm_tune_1) %>% 
  cbind(preproc_1) %>% 
  as_tibble() 

svm_pred_1 <- 
  collect_predictions(svm_tune_1, summarize = TRUE) %>% 
  cbind(preproc_1) %>% 
  as_tibble() 

# ------------------------------------------------------------------------------
# PCA components for diagnostic plots 

rec_pca_1 <- 
  norm_rec_1 %>% 
  step_normalize(starts_with("x")) %>% 
  step_pca(starts_with("x"), num_comp = 5, id = "pca") %>% 
  prep()

pca_data_1 <- 
  rec_pca_1 %>% 
  bake(new_data = NULL) %>% 
  cbind(preproc_1) %>% 
  as_tibble() 

pca_var_1 <- 
  rec_pca_1 %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "cumulative percent variance") %>% 
  dplyr::select(value, component) %>% 
  cbind(preproc_1) %>% 
  as_tibble() 

# ------------------------------------------------------------------------------
# Collate results for this pre-processing configuration

res_1 <- ls(pattern = "(_metrics_1)|(_pred_1)")
save(list = res_1, file = "RData/preproc_results_1.RData", compress = TRUE)

res_pca_1 <- ls(pattern = "^pca_")
save(list = res_pca_1, file = "RData/pca_results_1.RData", compress = TRUE)

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}

