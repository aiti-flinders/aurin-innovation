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


infrastructure_in_geography <- function(data, map, geography, name, year) {

  st_join(data, map) %>%
    group_by(.data[[geography]]) %>%
    tally(name = name) %>%
    st_drop_geometry()

}


education_location_2011 <- full_join(infrastructure_in_geography(unis, sa2_2011, "sa2_name", "unis", 2011),
                                     infrastructure_in_geography(tafes, sa2_2011, "sa2_name", "tafes", 2011),
                                     by = c("sa2_name")) %>%
  replace_na(list(unis = 0,
                  tafes = 0)) %>%
  mutate(year = 2011)



education_location_2016 <- full_join(infrastructure_in_geography(unis, sa2_2016, "sa2_name", "unis", 2016),
                                     infrastructure_in_geography(tafes, sa2_2016, "sa2_name", "tafes", 2016),
                                     by = c("sa2_name")) %>%
  replace_na(list(unis = 0,
                  tafes = 0)) %>%
  mutate(year = 2016)


usethis::use_data(education_location_2011, compress = "xz", overwrite = TRUE)
usethis::use_data(education_location_2016, compress = "xz", overwrite = TRUE)


}
