## code to prepare `sa2_occupation` dataset goes here
library(vroom)
library(tidyverse)

sa2_occp4_2016 <- vroom("data-raw/sa2_pow_occp4_2016.csv",
                        skip = 10,
                        n_max = 1105136,
                        col_select = c("sa2_name" = `SA2 (POW)`,
                                       "anzsco_name" = `OCCP - 4 Digit Level`,
                                       "employment" = `...3`),
                        col_types = "ccn") %>%
  fill(sa2_name, .direction = "down")  %>%
  mutate(year = 2016) %>%
  check_sa2(geography = "sa2_name",
            other = "anzsco_name") %>%
  group_by(sa2_name) %>%
  mutate(sa2_employment = sum(employment)) %>%
  filter(sa2_employment >= 150) %>%
  ungroup()

usethis::use_data(sa2_occp4_2016, overwrite = TRUE)
