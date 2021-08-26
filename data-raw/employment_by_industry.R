## code to prepare `employment_by_industry` dataset goes here
library(readr)
library(tidyr)
library(stringr)
library(dplyr)

emp_ind_sa2 <- function(path, skip, n_max, year) {


  read_csv({{path}}, skip = {{skip}}, n_max = {{n_max}}) %>%
    slice(-1) %>%
    rename("sa2_name" = 1) %>%
    pivot_longer(names_to = "industry_2",
                 values_to = "employment",
                 cols = where(is.double)) %>%
    filter(!str_detect(sa2_name, "Migratory - Offshore - Shipping"),
           !str_detect(sa2_name, "POW No Fixed Address"),
           !str_detect(sa2_name, "POW not applicable"),
           sa2_name != "Total",
           !industry_2 %in% c("Inadequately described",
                              "Not stated",
                              "Not applicable",
                              "Total")) %>%
    mutate(year = {{year}}) %>%
    group_by(sa2_name) %>%
    mutate(region_employment = sum(employment)) %>%
    ungroup() %>%
    group_by(industry_2) %>%
    mutate(industry_employment = sum(employment)) %>%
    ungroup()



}

sa2_indp2_2016 <- emp_ind_sa2("data-raw/sa2_pow_indp2_2016.csv",
                              skip = 9,
                              n_max = 2313,
                              year = 2016)

sa2_indp2_2011 <- emp_ind_sa2("data-raw/sa2_pow_indp2_2011.csv",
                              skip = 9,
                              n_max = 2236,
                              year = 2011)



usethis::use_data(sa2_indp2_2011, internal = TRUE, overwrite = TRUE)
usethis::use_data(sa2_indp2_2016, internal = TRUE, overwrite = TRUE)
