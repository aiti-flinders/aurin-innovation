## code to prepare `industrial_growth_opportunities` dataset goes here
#Industrial growth opportunities links state economic complexity data with SA2 industry comparative advantage data

library(ecomplexity)
library(purrr)
library(strayr)
library(dplyr)

devtools::load_all()

iga <- function(year) {

  states <- c("NSW",
              "VIC",
              "QLD",
              "SA",
              "WA",
              "TAS",
              "NT",
              "ACT")

  ica_with_state <- industry_comparative_advantage %>%
    left_join(read_absmap(area = "sa2", year = {{year}}, remove_year_suffix = TRUE)) %>%
    filter(year == {{year}}) %>%
    select(sa2_name, sa2_code, industry, comparative_advantage, year, state_name)

  state_ec <- get_data(states, years = {{year}}) %>%
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
    left_join(ica_with_state, by = c("location_code" = "state_name",
                                     "anzsic_subdivision" = "industry",
                                     "year")) %>%
    filter(comparative_advantage >= 1) %>%
    distinct() %>%
    add_product_names(digits = "4") %>%
    arrange(sa2_name, sa2_code) %>%
    select(Year = year,
           "Statistical Area 2 Code" = sa2_code,
           "Statistical Area 2 Name" = sa2_name,
           "Product Opportunity" = hs_product_name_short_en,
           "Product Code" = hs_product_code,
           "Product Development Benefit" = cog,
           "Product Industry" = anzsic_subdivision,
           "Region Industry Comparative Advantage" = comparative_advantage,
           State = location_code,
           "State Export Value" = export_value,
           "State Export Comparative Advantage" = rca)

  return(industrial_growth_opportunities)




}

industrial_growth_opportunities <- map_df(.x = c("2011", "2016"), .f = ~iga(.x))


usethis::use_data(industrial_growth_opportunities, overwrite = TRUE, compress = "xz")
