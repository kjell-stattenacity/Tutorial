library(prospectr)
library(caret)
library(knitr)
library(flextable)
library(officer)
library(tidyverse)
library(readxl)
library(ModelMetrics)
library(recipes)
library(readxl)
library(RColorBrewer)
library(gridExtra)
library(pls)
library(tidymodels)
library(plsmod)
library(rules)
library(data.table)

library(doParallel)
registerDoParallel(detectCores(logical = FALSE) - 1)

#LOC <- "/Users/kjelljohnson/My Drive/Projects/Tutorial"

LOC <- "/Users/kj/Google Drive/Projects/Tutorial"

dp_data <-
  read_csv(file.path(LOC, "dp_modeling_data.csv"))

dp_data_long <-
  dp_data %>%
  pivot_longer(`200`:`3300`, names_to="spec_num", values_to="value") %>%
  mutate(spec_num = as.numeric(spec_num))

# ------------------------------------------------------------------------------
#SG Transforms
dp_data_wide <- 
  pivot_wider(data=dp_data_long, names_from=spec_num, values_from=value)

dp_data_wide_sg <- 
  dp_data_wide %>%
  dplyr::select(-c(Sample, dp_conc))

dp_data_wide_sg_ID <- 
  dp_data_wide %>%
  dplyr::select(c(Sample, dp_conc))

dp_data_sg_1_2_15 <- 
  savitzkyGolay(X=dp_data_wide_sg, m=1, p=2, w=15)

dp_data_sg_1_2_15 <- as.tibble(dp_data_sg_1_2_15)
dp_data_sg_1_2_15 <- bind_cols(dp_data_wide_sg_ID, dp_data_sg_1_2_15)

dp_data_sg_long_1_2_15 <-
  dp_data_sg_1_2_15 %>%
  pivot_longer(`207`:`3293`, names_to="spec_num", values_to="value") %>%
  mutate(spec_num = as.numeric(spec_num))

###
dp_data_sg_2_2_15 <- 
  savitzkyGolay(X=dp_data_wide_sg, m=2, p=2, w=15)

dp_data_sg_2_2_15 <- as.tibble(dp_data_sg_2_2_15)
dp_data_sg_2_2_15 <- bind_cols(dp_data_wide_sg_ID, dp_data_sg_2_2_15)

dp_data_sg_long_2_2_15 <-
  dp_data_sg_2_2_15 %>%
  pivot_longer(`207`:`3293`, names_to="spec_num", values_to="value") %>%
  mutate(spec_num = as.numeric(spec_num))

###
dp_data_sg_1_2_49 <- 
  savitzkyGolay(X=dp_data_wide_sg, m=1, p=2, w=49)

dp_data_sg_1_2_49 <- as.tibble(dp_data_sg_1_2_49)
dp_data_sg_1_2_49 <- bind_cols(dp_data_wide_sg_ID, dp_data_sg_1_2_49)

dp_data_sg_long_1_2_49 <-
  dp_data_sg_1_2_49 %>%
  pivot_longer(`224`:`3276`, names_to="spec_num", values_to="value") %>%
  mutate(spec_num = as.numeric(spec_num))

###
dp_data_sg_2_2_49 <- 
  savitzkyGolay(X=dp_data_wide_sg, m=2, p=2, w=49)

dp_data_sg_2_2_49 <- as.tibble(dp_data_sg_2_2_49)
dp_data_sg_2_2_49 <- bind_cols(dp_data_wide_sg_ID, dp_data_sg_2_2_49)

dp_data_sg_long_2_2_49 <-
  dp_data_sg_2_2_49 %>%
  pivot_longer(`224`:`3276`, names_to="spec_num", values_to="value") %>%
  mutate(spec_num = as.numeric(spec_num))

# ------------------------------------------------------------------------------
###Training and Test splits
set.seed(910)
dp_data_split <-
  initial_split(dp_modeling_data, prop=0.77, strata=dp_conc)

original_train <- training(dp_data_split)
original_test <- testing(dp_data_split)

set.seed(910)
sg_1_2_15_split <-
  initial_split(dp_data_sg_1_2_15, prop=0.77, strata=dp_conc)

sg_1_2_15_train <- training(sg_1_2_15_split)
sg_1_2_15_test <- testing(sg_1_2_15_split)

set.seed(910)
sg_2_2_15_split <-
  initial_split(dp_data_sg_2_2_15, prop=0.77, strata=dp_conc)

sg_2_2_15_train <- training(sg_2_2_15_split)
sg_2_2_15_test <- testing(sg_2_2_15_split)

set.seed(910)
sg_1_2_49_split <-
  initial_split(dp_data_sg_1_2_49, prop=0.77, strata=dp_conc)

sg_1_2_49_train <- training(sg_1_2_49_split)
sg_1_2_49_test <- testing(sg_1_2_49_split)

set.seed(910)
sg_2_2_49_split <-
  initial_split(dp_data_sg_2_2_49, prop=0.77, strata=dp_conc)

sg_2_2_49_train <- training(sg_2_2_49_split)
sg_2_2_49_test <- testing(sg_2_2_49_split)


# ------------------------------------------------------------------------------
# Function to create figures of Obs. vs Pred and Obs. vs Resid

fit_figures <- function(fit_data, RESPONSE) {
  # Generate observed vs predicted and observed versus residual plots
  obs_vs_pred <-
    ggplot(fit_data, aes(y=!!RESPONSE, .pred)) +
    geom_smooth(se = FALSE, color = "black", method = 'rlm', formula = y ~ x) +
    geom_point(alpha = 0.75, color = "#387291", size = 1.5) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2) +
    xlab(quo_name(RESPONSE)) +
    ylab(paste0("Predicted ", quo_name(RESPONSE))) +
    ggtitle("Observed vs. Predicted")
  
  obs_vs_resid <-
    ggplot(fit_data, aes(!!RESPONSE, resid)) +
    geom_hline(yintercept=0, linetype=2, size=1.1) +
    geom_point(alpha = 0.75, color = "#387291", size = 1.5) +
    xlab(quo_name(RESPONSE)) +
    ylab("Residual") +
    ggtitle("Observed vs. Residual")
  
  grid.arrange(obs_vs_pred, obs_vs_resid, ncol=2)
}

# ------------------------------------------------------------------------------
# PLS
pls_modeling_fcn <- function(TRAINDATA, TESTDATA, RESPONSE, SPLIT){

  current_formula = as.formula(paste0("`",quo_name(RESPONSE), "`", "~ ."))
  
  current_recipe <-
    recipe(current_formula, data = TRAINDATA)
  
  # 5 repeats of 10-fold cross-validation
  set.seed(522)
  cv_folds <- 
    vfold_cv(TRAINDATA, v=10, repeats=5)
  
  pls_model <-
    parsnip::pls(num_comp = tune()) %>%
    set_engine("mixOmics") %>%
    set_mode("regression")
  
  pls_workflow <-
    workflow() %>%
    add_recipe(current_recipe) %>%
    add_model(pls_model)
  
  grid_ctrl <- control_grid(save_pred = TRUE,  # Save the hold-out predictions
                            allow_par = TRUE,  # Allow parallel processing
                            parallel_over = "everything")
  
  # Perform cross-validation for the workflow and specify corresponding controls
  pls_tune <-
    tune_grid(pls_workflow,
              resamples = cv_folds, 
              grid = tibble(num_comp = 1:15),
              control = grid_ctrl)
  
  # Select the model with the optimal CV RMSE
  pls_best_model <-
    select_best(pls_tune, metric="rmse") %>%
    dplyr::select(-.config)
  
  pls_tune_metrics <- 
    pls_tune %>%
    collect_metrics()
  
  # Get the hold-out predictions from the best model and calculate the residual
  pls_cv_preds <-
    collect_predictions(pls_tune, summarize = TRUE) %>%
    mutate(resid = !!RESPONSE - .pred) %>%
    dplyr::filter(num_comp == pls_best_model$num_comp) %>%
    dplyr::select(-.config)
  
  #predict the test set
  final_model <-
    finalize_workflow(
      pls_workflow,
      parameters = pls_best_model
    )
  
  pls_tuned_fit <-
    last_fit(final_model, split=SPLIT)
  
  pls_test_preds <-
    pls_tuned_fit$.predictions[[1]] %>%
    dplyr::select(-.config)
  
  list(tune_info = pls_tune,
       tune_metrics = pls_tune_metrics,
       cv_predictions = pls_cv_preds,
       test_predictions = pls_test_preds)
}

original_data_pls <-
  pls_modeling_fcn(TRAINDATA=original_train %>% dplyr::select(-Sample), 
                 TESTDATA=original_test %>% dplyr::select(-Sample), 
                 RESPONSE=quo(dp_conc), 
                 SPLIT=dp_data_split)
  
sg_1_2_15_pls <-
  pls_modeling_fcn(TRAINDATA=sg_1_2_15_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_1_2_15_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_1_2_15_split)

sg_1_2_49_pls <-
  pls_modeling_fcn(TRAINDATA=sg_1_2_49_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_1_2_49_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_1_2_49_split)

sg_2_2_15_pls <-
  pls_modeling_fcn(TRAINDATA=sg_2_2_15_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_2_2_15_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_2_2_15_split)

sg_2_2_49_pls <-
  pls_modeling_fcn(TRAINDATA=sg_2_2_49_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_2_2_49_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_2_2_49_split)

#---------------------------------------------------------------------------------
# RF
rf_modeling_fcn <- function(TRAINDATA, TESTDATA, RESPONSE, SPLIT){

  current_formula = as.formula(paste0("`",quo_name(RESPONSE), "`", "~ ."))
  
  current_recipe <-
    recipe(current_formula, data = TRAINDATA)
  
  # 5 repeats of 10-fold cross-validation
  set.seed(522)
  cv_folds <- 
    vfold_cv(TRAINDATA, v=10, repeats=5)
  
  rf_model <- 
    rand_forest(mtry = tune(),
                trees = 1000,
                min_n = 8) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  rf_workflow <-
    workflow() %>%
    add_recipe(current_recipe) %>%
    add_model(rf_model)
  
  grid_ctrl <- control_grid(save_pred = TRUE,  # Save the hold-out predictions
                            allow_par = TRUE,  # Allow parallel processing
                            parallel_over = "everything")
  
  # Perform cross-validation for the workflow and specify corresponding controls
  rf_tune <-
    tune_grid(rf_workflow,
              resamples = cv_folds, 
              grid = tibble(mtry = c(10, 20, 30, 35, 40, 45, 50, 100)),
              control = grid_ctrl)
  
  # Select the model with the optimal CV RMSE
  rf_best_model <-
    select_best(rf_tune, metric="rmse") %>%
    dplyr::select(-.config)
  
  rf_tune_metrics <- 
    rf_tune %>%
    collect_metrics()
  
  # Get the hold-out predictions from the best model and calculate the residual
  rf_cv_preds <-
    collect_predictions(rf_tune, summarize = TRUE) %>%
    mutate(resid = !!RESPONSE - .pred) %>%
    dplyr::filter(mtry == rf_best_model$mtry) %>%
    dplyr::select(-.config)

  #predict the test set
  final_model <-
    finalize_workflow(
      rf_workflow,
      parameters = rf_best_model
    )
  
  rf_tuned_fit <-
    last_fit(final_model, split=SPLIT)
  
  rf_test_preds <-
    rf_tuned_fit$.predictions[[1]] %>%
    dplyr::select(-.config)
  
  list(tune_info = rf_tune,
       tune_metrics = rf_tune_metrics,
       cv_predictions = rf_cv_preds,
       test_predictions = rf_test_preds)
}

original_data_rf <-
  rf_modeling_fcn(TRAINDATA=original_train %>% dplyr::select(-Sample), 
                   TESTDATA=original_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=dp_data_split)

sg_1_2_15_rf <-
  rf_modeling_fcn(TRAINDATA=sg_1_2_15_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_1_2_15_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_1_2_15_split)

sg_1_2_49_rf <-
  rf_modeling_fcn(TRAINDATA=sg_1_2_49_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_1_2_49_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_1_2_49_split)

sg_2_2_15_rf <-
  rf_modeling_fcn(TRAINDATA=sg_2_2_15_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_2_2_15_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_2_2_15_split)

sg_2_2_49_rf <-
  rf_modeling_fcn(TRAINDATA=sg_2_2_49_train %>% dplyr::select(-Sample), 
                   TESTDATA=sg_2_2_49_test %>% dplyr::select(-Sample), 
                   RESPONSE=quo(dp_conc), 
                   SPLIT=sg_2_2_49_split)

cubist_modeling_fcn <- function(TRAINDATA, TESTDATA, RESPONSE, SPLIT){

  current_formula = as.formula(paste0("`",quo_name(RESPONSE), "`", "~ ."))
  
  current_recipe <-
    recipe(current_formula, data = TRAINDATA)
  
  # 5 repeats of 10-fold cross-validation
  set.seed(522)
  cv_folds <- 
    vfold_cv(TRAINDATA, v=10, repeats=5)
  
  cubist_model <- 
    cubist_rules(committees = tune(), neighbors = tune()) %>%
    set_engine("Cubist") %>%
    set_mode("regression")
  
  cubist_workflow <-
    workflow() %>%
    add_recipe(current_recipe) %>%
    add_model(cubist_model)
  
  grid_ctrl <- control_grid(save_pred = TRUE,
                            allow_par = TRUE,
                            parallel_over = "everything")
  
  # Perform cross-validation for the workflow and specify corresponding controls
  cubist_tune <-
    tune_grid(cubist_workflow,
              resamples = cv_folds, 
              grid = crossing(committees = c(5,10,15,20,25,30), neighbors = c(3,5,7)),
              control = grid_ctrl)
  
  # Select the model with the optimal CV RMSE
  cubist_best_model <-
    select_best(cubist_tune, metric="rmse") %>%
    dplyr::select(-.config)
  
  cubist_tune_metrics <- 
    cubist_tune %>%
    collect_metrics()
  
  # Get the hold-out predictions from the best model and calculate the residual
  cubist_cv_preds <-
    collect_predictions(cubist_tune, summarize = TRUE) %>%
    mutate(resid = !!RESPONSE - .pred)  %>%
    dplyr::filter(committees == cubist_best_model$committees & neighbors == cubist_best_model$neighbors) %>%
    dplyr::select(-.config)

  #predict the test set
  final_model <-
    finalize_workflow(
      cubist_workflow,
      parameters = cubist_best_model
    )
  
  cubist_tuned_fit <-
    last_fit(final_model, split=SPLIT)
  
  cubist_test_preds <-
    cubist_tuned_fit$.predictions[[1]] %>%
    dplyr::select(-.config)
  
  list(tune_info = cubist_tune,
       tune_metrics = cubist_tune_metrics,
       cv_predictions = cubist_cv_preds,
       test_predictions = cubist_test_preds)
}

original_data_cubist <-
  cubist_modeling_fcn(TRAINDATA=original_train %>% dplyr::select(-Sample), 
                  TESTDATA=original_test %>% dplyr::select(-Sample), 
                  RESPONSE=quo(dp_conc), 
                  SPLIT=dp_data_split)

sg_1_2_15_cubist <-
  cubist_modeling_fcn(TRAINDATA=sg_1_2_15_train %>% dplyr::select(-Sample), 
                  TESTDATA=sg_1_2_15_test %>% dplyr::select(-Sample), 
                  RESPONSE=quo(dp_conc), 
                  SPLIT=sg_1_2_15_split)

sg_1_2_49_cubist <-
  cubist_modeling_fcn(TRAINDATA=sg_1_2_49_train %>% dplyr::select(-Sample), 
                  TESTDATA=sg_1_2_49_test %>% dplyr::select(-Sample), 
                  RESPONSE=quo(dp_conc), 
                  SPLIT=sg_1_2_49_split)

sg_2_2_15_cubist <-
  cubist_modeling_fcn(TRAINDATA=sg_2_2_15_train %>% dplyr::select(-Sample), 
                  TESTDATA=sg_2_2_15_test %>% dplyr::select(-Sample), 
                  RESPONSE=quo(dp_conc), 
                  SPLIT=sg_2_2_15_split)

sg_2_2_49_cubist <-
  cubist_modeling_fcn(TRAINDATA=sg_2_2_49_train %>% dplyr::select(-Sample), 
                  TESTDATA=sg_2_2_49_test %>% dplyr::select(-Sample), 
                  RESPONSE=quo(dp_conc), 
                  SPLIT=sg_2_2_49_split)


svm_modeling_fcn <- function(TRAINDATA, TESTDATA, RESPONSE, SPLIT){

  current_formula = as.formula(paste0("`",quo_name(RESPONSE), "`", "~ ."))
  
  current_recipe <-
    recipe(current_formula, data = TRAINDATA)
  
  # 5 repeats of 10-fold cross-validation
  set.seed(522)
  cv_folds <- 
    vfold_cv(TRAINDATA, v=10, repeats=5)
  
  svm_model <- 
    svm_rbf(cost = tune(),
            rbf_sigma = tune()) %>%
    set_mode("regression") %>%
    set_engine("kernlab")
  
  svm_workflow <-
    workflow() %>%
    add_recipe(current_recipe) %>%
    add_model(svm_model)
  
  grid_ctrl <- control_grid(save_pred = TRUE,
                            allow_par = TRUE,
                            parallel_over = "everything")
  
  # Perform cross-validation for the workflow and specify corresponding controls
  svm_tune <-
    tune_grid(svm_workflow,
              resamples = cv_folds, 
              grid = crossing(cost = 10^c(0:4), rbf_sigma = 10^c(-7:-1)),
              control = grid_ctrl)
  
  # Select the model with the optimal CV RMSE
  svm_best_model <-
    select_best(svm_tune, metric="rmse") %>%
    dplyr::select(-.config)

  svm_tune_metrics <- 
    svm_tune %>%
    collect_metrics()
  
  # Get the hold-out predictions from the best model and calculate the residual
  svm_cv_preds <-
    collect_predictions(svm_tune, summarize = TRUE) %>%
    mutate(resid = !!RESPONSE - .pred)  %>%
    dplyr::filter(cost == svm_best_model$cost & rbf_sigma == svm_best_model$rbf_sigma) %>%
    dplyr::select(-.config)
  

  #predict the test set
  final_model <-
    finalize_workflow(
      svm_workflow,
      parameters = svm_best_model
    )

  svm_tuned_fit <-
    last_fit(final_model, split=SPLIT)
  
  svm_test_preds <-
    svm_tuned_fit$.predictions[[1]] %>%
    dplyr::select(-.config)
  
  list(tune_info = svm_tune,
       tune_metrics = svm_tune_metrics,
       cv_predictions = svm_cv_preds,
       test_predictions = svm_test_preds)
}

original_data_svm <-
  svm_modeling_fcn(TRAINDATA=original_train %>% dplyr::select(-Sample), 
                      TESTDATA=original_test %>% dplyr::select(-Sample), 
                      RESPONSE=quo(dp_conc), 
                      SPLIT=dp_data_split)

sg_1_2_15_svm <-
  svm_modeling_fcn(TRAINDATA=sg_1_2_15_train %>% dplyr::select(-Sample), 
                      TESTDATA=sg_1_2_15_test %>% dplyr::select(-Sample), 
                      RESPONSE=quo(dp_conc), 
                      SPLIT=sg_1_2_15_split)

sg_1_2_49_svm <-
  svm_modeling_fcn(TRAINDATA=sg_1_2_49_train %>% dplyr::select(-Sample), 
                      TESTDATA=sg_1_2_49_test %>% dplyr::select(-Sample), 
                      RESPONSE=quo(dp_conc), 
                      SPLIT=sg_1_2_49_split)

sg_2_2_15_svm <-
  svm_modeling_fcn(TRAINDATA=sg_2_2_15_train %>% dplyr::select(-Sample), 
                      TESTDATA=sg_2_2_15_test %>% dplyr::select(-Sample), 
                      RESPONSE=quo(dp_conc), 
                      SPLIT=sg_2_2_15_split)

sg_2_2_49_svm <-
  svm_modeling_fcn(TRAINDATA=sg_2_2_49_train %>% dplyr::select(-Sample), 
                      TESTDATA=sg_2_2_49_test %>% dplyr::select(-Sample), 
                      RESPONSE=quo(dp_conc), 
                      SPLIT=sg_2_2_49_split)

#save.image(file=file.path(LOC, "tutorial.Rdata"))
