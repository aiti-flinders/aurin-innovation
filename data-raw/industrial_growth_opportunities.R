## code to prepare `industrial_growth_opportunities` dataset goes here
#Industrial growth opportunities links state economic complexity data with SA2 industry comparative advantage data

library(ecomplexity)
library(strayr)
library(dplyr)

devtools::load_all()

ica_with_state <- industry_comparative_advantage %>%
  left_join(read_absmap(area = "sa2", year = "2016")) %>%
  select(sa2_name, sa2_code_2016, industry, comparative_advantage, year, state_name_2016)

states <- c("NSW",
            "VIC",
            "QLD",
            "SA",
            "WA",
            "TAS",
            "NT",
            "ACT")

state_ec <- get_data(states, year = 2016)

#1 - Keep only ICA >=1
#2 - RCA product < 0
#3 - Export product > 0
#4 - COG > 0

#usethis::use_data(industrial_growth_opportunities, overwrite = TRUE)
