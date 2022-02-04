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

state_ec <- get_data(states, years = 2016) %>%
  mutate(location_code = clean_state(location_code, to = "state_name"))

#1 - Keep only ICA >=1
#2 - RCA product < 0
#3 - Export product > 0
#4 - COG > 0

industrial_growth_opportunities <- state_ec %>%
  filter(export_value > 0,
         cog > 0,
         rca < 1) %>%
  left_join(anzsic_hs, by = "hs_product_code") %>%
  left_join(ica_with_state, by = c("location_code" = "state_name_2016",
                                   "anzsic_subdivision" = "industry",
                                   "year")) %>%
  filter(comparative_advantage >= 1) %>%
  add_product_names(digits = "4") %>%
  arrange(sa2_name, sa2_code_2016)



usethis::use_data(industrial_growth_opportunities, overwrite = TRUE)
