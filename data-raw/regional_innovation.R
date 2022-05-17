## code to prepare `regional_innovation` dataset goes here
library(aurininnovation)
library(purrr)

regional_innovation <-
  map_dfr(.x = c(2011, 2016),
          .f = ~create_regional_innovation(year = .x, geography = "sa2", adjust = FALSE))

usethis::use_data(regional_innovation, overwrite = TRUE)
