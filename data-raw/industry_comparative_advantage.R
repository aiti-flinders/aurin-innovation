## code to prepare `industry_comparative_advantage` dataset goes here
library(dplyr)
library(aurininnovation)


industry_comparative_advantage <- bind_rows(
  location_quotient(sa2_indp2_2016,
                    options = list(x = "sa2_name", y = "industry_2", total = "region_employment", value = "employment")) %>% mutate(year = 2016),
  location_quotient(sa2_indp2_2011,
                    options = list(x = "sa2_name", y = "industry_2", total = "region_employment", value = "employment")) %>% mutate(year = 2011)
)



usethis::use_data(industry_comparative_advantage, overwrite = TRUE)
