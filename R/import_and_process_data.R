library(tidymodels)
library(readr)
library(janitor)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------
# Import raw data then re-org the dat ain different ways

dp_raw <-
  read_csv(file.path("data", "dp_modeling_data.csv")) %>% 
  rename(sample_id = Sample, concentration = dp_conc)

data_long <-
  dp_raw %>%
  pivot_longer(`200`:`3300`, names_to = "wave_number", values_to = "intensity") %>%
  mutate(wave_number = as.numeric(wave_number))

data_wide <-
  pivot_wider(data = data_long,
              names_from = wave_number,
              values_from = intensity) %>% 
  clean_names()

save(data_long, file = "RData/data_long.RData")
save(data_wide, file = "RData/data_wide.RData")

# ------------------------------------------------------------------------------
# Now make different versions of the data use Savitzky-Golay with different
# parameters

savitzky_golay <- function(x, diffs, poly, window) {
  require(prospectr)
  
  measurment_only <- x %>% dplyr::select(starts_with("x"))
  
  other_columns <- x %>% dplyr::select(-starts_with("x"))
  
  # Process analytical measurements
  savitzkyGolay(X = measurment_only, m = diffs, p = poly, w = window) %>% 
    as_tibble() %>% 
    # Bring back the other data
    bind_cols(other_columns) %>%
    # Add information on the pre-processing
    mutate(
      differentiation_order = diffs, 
      polynomial_order = poly,
      window_size = window
    ) %>% 
    relocate(-starts_with("x"))
}

processed_data <- 
  bind_rows(
    savitzky_golay(data_wide, diffs = 1, poly = 2, window = 15),
    savitzky_golay(data_wide, diffs = 2, poly = 2, window = 15),
    savitzky_golay(data_wide, diffs = 1, poly = 2, window = 49),
    savitzky_golay(data_wide, diffs = 2, poly = 2, window = 49)
  )

save(processed_data, file = "RData/processed_data.RData")
