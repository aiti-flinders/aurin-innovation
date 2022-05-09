## code to prepare `abs_maps` dataset goes here
library(strayr)

sa2_2016 <- read_absmap("sa22016", remove_year_suffix = TRUE)
sa2_2011 <- read_absmap("sa22011", remove_year_suffix = TRUE)

usethis::use_data(sa2_2016, overwrite = TRUE)
usethis::use_data(sa2_2011, overwrite = TRUE)

