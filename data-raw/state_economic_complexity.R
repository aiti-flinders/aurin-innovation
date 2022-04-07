## code to prepare `state_economic_complexity` dataset goes here
library(ecomplexity)
library(dplyr)
library(strayr)

state_economic_complexity <- get_data(c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")) %>%
  mutate(location_code = clean_state(location_code, to = "state_name"))



usethis::use_data(state_economic_complexity, compress = "xz", overwrite = TRUE)
