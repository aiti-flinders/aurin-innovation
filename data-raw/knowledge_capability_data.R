## code to prepare `knowledge_capability_data` dataset goes here
library(magrittr)
library(aurininnovation)

knowledge_capability_data <- create_kc() %>%
  add_sa2_codes()

usethis::use_data(knowledge_capability_data, compress = "xz", overwrite = TRUE)
