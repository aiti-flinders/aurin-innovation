## code to prepare `sa2_pow_siemp` dataset goes here
library(vroom)
library(tidyverse)

sa2_siemp_2016 <- vroom("data-raw/sa2_pow_siemp.csv",
                       skip = 9,
                       n_max = 25432,
                       col_select = c("sa2_name" = `SA2 (POW)`,
                                      "employment_status" = `SIEMP Status in Employment`,
                                      "employment" = `Count`)) %>%
  check_sa2(geography =  "sa2_name",
            other = "employment_status") %>%
  group_by(sa2_name) %>%
  mutate(total_employment = sum(employment)) %>%
  ungroup() %>%
  select(-total_employment)

usethis::use_data(sa2_siemp_2016, compress = "xz", overwrite = TRUE)
