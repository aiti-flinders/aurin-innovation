## code to prepare `kibs_industry_class` dataset goes here
kibs_industry_class <- kibs()

usethis::use_data(kibs_industry_class,compress = "xz", overwrite = TRUE)
