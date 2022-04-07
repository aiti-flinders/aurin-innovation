## code to prepare `economic_complexity_products` dataset goes here
library(tibble)
load("data-raw/hs_product.RData")

#Clear stata attributes
attr(table$product_id, "format.stata") <- NULL
attr(table$hs_product_code, "format.stata") <- NULL
attr(table$hs_product_name_short_en, "format.stata") <- NULL
attr(table$level, "format.stata") <- NULL
attr(table$parent_id, "format.stata") <- NULL

product_data <- table %>%
  as_tibble()


usethis::use_data(product_data, compress = 'xz', overwrite = TRUE)

