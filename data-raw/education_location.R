## code to prepare `education_location` dataset goes here
library(aurintools)
library(strayr)
library(tidyverse)

if (any(!file.exists(c("data-raw/unis.geoJSON", "data-raw/tafes.geoJSON")))) {

uni_id <- aurin_id("AURIN - National Education Facilities - Universities (Point) 2018", exact = TRUE)
tafe_id <- aurin_id("AURIN - National Education Facilities - TAFEs (Point) 2018", exact = TRUE)

map2_chr(.x = c(uni_id, tafe_id),
         .y = c("unis", "tafes"),
         .f = ~aurin_download_file(api_id = .x, out_file_name = .y, out_folder = "data-raw"))

} else {


unis <- read_aurin("data-raw/unis.geoJSON") %>%
  st_transform(4326)

tafes <- read_aurin("data-raw/tafes.geoJSON") %>%
  st_transform(4326)

sa2 <- read_absmap("sa22016", remove_year_suffix = TRUE)

unis_in_sa2 <- st_join(unis, sa2) %>%
  group_by(sa2_name) %>%
  tally(name = "unis") %>%
  st_drop_geometry()

tafes_in_sa2 <- st_join(tafes, sa2) %>%
  group_by(sa2_name) %>%
  tally(name = "tafes") %>%
  st_drop_geometry()

education_location <- left_join(sa2, unis_in_sa2) %>%
  left_join(tafes_in_sa2) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(sa2_name, unis, tafes) %>%
  replace_na(list(unis = 0, tafes = 0, infrastructure = 0)) %>%
  mutate(infrastructure = unis + tafes) %>%
  check_sa2(geography = "sa2_name", other = "unis")


usethis::use_data(education_location, compress = "xz", overwrite = TRUE)

}
