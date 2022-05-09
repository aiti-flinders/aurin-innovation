## code to prepare `abs_maps` dataset goes here
library(strayr)
library(absmapsdata)
library(dplyr)

sa2_2016 <- read_absmap("sa22016", remove_year_suffix = TRUE)
sa2_2011 <- read_absmap("sa22011", remove_year_suffix = TRUE)

sa2_2016_to_sa2_2011 <- get_correspondence_absmaps("sa2", 2016, "sa2", 2011) %>%
  select(sa2_name_2016 = SA2_NAME_2016,
         sa2_name_2011 = SA2_NAME_2011,
         ratio)


usethis::use_data(sa2_2016, compress = "bzip2", overwrite = TRUE)
usethis::use_data(sa2_2011, compress = "bzip2", overwrite = TRUE)
usethis::use_data(sa2_2016_to_sa2_2011, compress = "bzip2", overwrite = TRUE)

