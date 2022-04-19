## code to prepare `table_builder_data` dataset goes here.
library(readr)
library(tidyr)
library(stringr)
library(dplyr)

# SA2 POW HEAP1 (2016, 2011)

sa2_pow_heap <- function(path, skip, n_max, year) {

  read_csv(file = path,
           skip = {{skip}},
           n_max = {{n_max}},
           col_select = c(sa2_name = `SA2 (POW)`,
                          education_level = `HEAP - 1 Digit Level`,
                          employment = `Count`),
           show_col_types = FALSE) %>%
    check_sa2(geography = "sa2_name",
              other = "education_level")  %>%
    mutate(year = {{year}})

}

sa2_heap1_2016 <- sa2_pow_heap("data-raw/sa2_pow_heap1_2016.csv",
                               skip = 9,
                               n_max = 27745,
                               year = 2016)


usethis::use_data(sa2_heap1_2016, compress = "xz", overwrite = TRUE)

# SA2 POW OCCP4 (2016, 2011)

sa2_pow_occp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = `SA2 (POW)`,
                          anzsco_name = `OCCP - 4 Digit Level`,
                          employment = `...3`),
           col_types = "ccn") %>%
    fill(sa2_name, .direction = "down") %>%
    check_sa2(geography = "sa2_name",
              other = "anzsco_name") %>%
    mutate(year = 2016)

}

sa2_occp4_2016 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2016.csv",
                                skip = 10,
                                n_max = 1105136,
                               year = 2016)

usethis::use_data(sa2_occp4_2016, compress = "xz", overwrite = TRUE)

# SA2 POW INDP4 (2016, 2011)

## code to prepare `sa2_indp4_2016` dataset goes here
sa2_pow_ind4 <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},  show_col_types = F,
           col_select = c(sa2_name = `SA2 (POW)`,
                          anzsic_class = `INDP - 4 Digit Level`,
                          employment = `Count`)) %>%
    check_sa2(geography = "sa2_name",
              other = "anzsic_class") %>%
    mutate(year = {{year}})

}

sa2_indp4_2016 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2016.csv",
                               skip = 9,
                               n_max = 1666952,
                               year = 2016)

usethis::use_data(sa2_indp4_2016, compress = "xz", overwrite = TRUE)

# SA2 POW INDP2 (2016, 2011)
sa2_pow_ind <- function(path, skip, n_max, year) {


  read_csv(path, skip = {{skip}}, n_max = {{n_max}}, show_col_types = F) %>%
    slice(-1) %>%
    rename("sa2_name" = 1) %>%
    pivot_longer(names_to = "industry",
                 values_to = "employment",
                 cols = where(is.double))  %>%
    check_sa2(geography = "sa2_name",
              other = "industry") %>%
    mutate(year = {{year}})


}

sa2_indp2_2016 <- sa2_pow_ind("data-raw/sa2_pow_indp2_2016.csv",
                              skip = 9,
                              n_max = 2313,
                              year = 2016)

sa2_indp2_2011 <- sa2_pow_ind("data-raw/sa2_pow_indp2_2011.csv",
                              skip = 9,
                              n_max = 2236,
                              year = 2011)



usethis::use_data(sa2_indp2_2011, compress = "xz", overwrite = TRUE)

usethis::use_data(sa2_indp2_2016, compress = "xz", overwrite = TRUE)

# SA2 POW QALFP2 (2016, 2011)

sa2_pow_qalfp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = `SA2 (POW)`,
                          qualification = `QALFP - 2 Digit Level`,
                          employment = Count)) %>%
    check_sa2(geography = "sa2_name",
              other = "qualification") %>%
    mutate(year = {{year}})

}

sa2_qalfp2_2016 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp.csv",
                                 skip = 9,
                                 n_max = 36992,
                                 year = 2016)

usethis::use_data(sa2_qalfp2_2016, compress = "xz", overwrite = TRUE)

# SA2 POW SIEMP (2016, 2011)

sa2_pow_siemp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = `SA2 (POW)`,
                          employment_status = `SIEMP Status in Employment`,
                          employment = `Count`)) %>%
    check_sa2(geography = "sa2_name",
              other = "employment_status") %>%
    mutate(year = {{year}})
}

sa2_siemp_2016 <- sa2_pow_siemp("data-raw/sa2_pow_siemp.csv",
                                skip = 9,
                                n_max = 25432,
                                year = 2016)

usethis::use_data(sa2_siemp_2016, compress = "xz", overwrite = TRUE)




