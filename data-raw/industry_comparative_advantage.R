## code to prepare `industry_comparative_advantage` dataset goes here
library(dplyr)
library(aurininnovation)
library(sf)
library(strayr)

industry_comparative_advantage <- bind_rows(
  location_quotient(sa2_indp2_2016,
                    options = list(x = "sa2_name",
                                   y = "industry_2",
                                   total = "region_employment",
                                   value = "employment")) %>%
    mutate(year = 2016) %>%
    left_join(read_absmap("sa22016"), by = c("sa2_name" = "sa2_name_2016")) %>%
    select(sa2_name, sa2_code_2016, industry, comparative_advantage = rca, year),
  location_quotient(sa2_indp2_2011,
                    options = list(x = "sa2_name",
                                   y = "industry_2",
                                   total = "region_employment",
                                   value = "employment")) %>%
    mutate(year = 2011) %>%
    left_join(read_absmap("sa22011"), by = c("sa2_name" = "sa2_name_2011")) %>%
    select(sa2_name, sa2_code_2011, industry, comparative_advantage = rca, year)
)



usethis::use_data(industry_comparative_advantage, overwrite = TRUE)
