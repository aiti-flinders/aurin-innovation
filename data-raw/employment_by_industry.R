## code to prepare `employment_by_industry` dataset goes here
library(readr)
library(tidyr)
library(stringr)
library(dplyr)

sa2_indp2 <- read_csv("data-raw/sa2_pow_indp2.csv",
                      skip = 9,
                      n_max = 2313) %>%
  slice(-1) %>%
  rename("sa2_name_2016" = 1) %>%
  filter(!str_detect(sa2_name_2016, "Migratory - Offshore - Shipping"),
         !str_detect(sa2_name_2016, "POW No Fixed Address"),
         !str_detect(sa2_name_2016, "POW not applicable"),
         sa2_name_2016 != "Total") %>%
  select(1:106) %>%
  pivot_longer(names_to = "industry_2",
               values_to = "employment",
               cols = where(is.double)) %>%
  group_by(sa2_name_2016) %>%
  mutate(region_employment = sum(employment)) %>%
  ungroup() %>%
  group_by(industry_2) %>%
  mutate(industry_employment = sum(employment)) %>%
  ungroup()




usethis::use_data(sa2_indp2, internal = TRUE, overwrite = TRUE)
