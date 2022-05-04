## code to prepare `table_builder_data` dataset goes here.
library(readr)
library(tidyr)
library(stringr)
library(dplyr)

# SA2 POW QALLP1 (2016, 2011)

sa2_pow_qallp <- function(path, skip, n_max, year) {

  read_csv(file = path,
           skip = {{skip}},
           n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          education_level = 3,
                          employment = 4),
           show_col_types = FALSE) %>%
    check_sa2(geography = "sa2_name",
              other = "education_level")  %>%
   mutate(year = {{year}})

}

sa2_qallp1_2016 <- sa2_pow_qallp("data-raw/sa2_pow_qallp_2016.csv",
                               skip = 9,
                               n_max = 20808,
                               year = 2016)

sa2_qallp1_2011 <- sa2_pow_qallp("data-raw/sa2_pow_qallp_2011.csv",
                               skip = 9,
                               n_max = 20115,
                               year = 2011)


usethis::use_data(sa2_qallp1_2011, compress = "xz", overwrite = TRUE)
usethis::use_data(sa2_qallp1_2016, compress = "xz", overwrite = TRUE)


# SA2 POW OCCP4 (2016, 2011)

sa2_pow_occp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          anzsco_name = 3,
                          employment = 4)) %>%
    fill(sa2_name, .direction = "down") %>%
    check_sa2(geography = "sa2_name",
              other = "anzsco_name") %>%
    mutate(year = {{year}})

}

sa2_occp4_2016 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2016.csv",
                                skip = 10,
                                n_max = 1105136,
                               year = 2016)

sa2_occp4_2011 <- sa2_pow_occp("data-raw/sa2_pow_occp4_2011.csv",
                               skip = 9,
                               n_max = 1068330,
                               year = 2011)

usethis::use_data(sa2_occp4_2016, compress = "xz", overwrite = TRUE)
usethis::use_data(sa2_occp4_2011, compress = "xz", overwrite = TRUE)

# SA2 POW INDP4 (2016, 2011)

## code to prepare `sa2_indp4_2016` dataset goes here
sa2_pow_ind4 <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},  show_col_types = F,
           col_select = c(sa2_name = 2,
                          anzsic_class = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "anzsic_class") %>%
    mutate(year = {{year}})

}

sa2_indp4_2016 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2016.csv",
                               skip = 9,
                               n_max = 1666952,
                               year = 2016)

sa2_indp4_2011 <- sa2_pow_ind4("data-raw/sa2_pow_indp4_2011.csv",
                               skip = 9,
                               n_max = 1574532,
                               year = 2011)

usethis::use_data(sa2_indp4_2016, compress = "xz", overwrite = TRUE)
usethis::use_data(sa2_indp4_2011, compress = "xz", overwrite = TRUE)

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
           col_select = c(sa2_name = 2,
                          qualification = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "qualification") %>%
    mutate(year = {{year}})

}

sa2_qalfp2_2016 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp_2016.csv",
                                 skip = 9,
                                 n_max = 36992,
                                 year = 2016)

sa2_qalfp2_2011 <- sa2_pow_qalfp("data-raw/sa2_pow_qalfp_2011.csv",
                                 skip = 9,
                                 n_max = 35760,
                                 year = 2011)

usethis::use_data(sa2_qalfp2_2016, compress = "xz", overwrite = TRUE)
usethis::use_data(sa2_qalfp2_2011, compress = "xz", overwrite = TRUE)

# SA2 POW SIEMP (2016, 2011)

sa2_pow_siemp <- function(path, skip, n_max, year) {

  read_csv(path, skip = {{skip}}, n_max = {{n_max}},
           col_select = c(sa2_name = 2,
                          employment_status = 3,
                          employment = 4)) %>%
    check_sa2(geography = "sa2_name",
              other = "employment_status") %>%
    mutate(year = {{year}})
}

#SIEMP was introduced in 2016 to replace 2011 - back cast to ensure they're equal.

sa2_siemp_2016 <- sa2_pow_siemp("data-raw/sa2_pow_siemp_2016.csv",
                                skip = 9,
                                n_max = 25432,
                                year = 2016) %>%
  pivot_wider(names_from = employment_status,
              values_from = employment) %>%
  mutate("Owner managers of incorporated enterprises" = `Owner manager of incorporated enterprise with employees` + `Owner manager of incorporated enterprise without employees` + `Owner manager of incorporated enterprise - employees not stated`,
         "Owner managers of unincorporated enterprises" = `Owner manager of unincorporated enterprise with employees` + `Owner manager of unincorporated enterprise without employees` + `Owner manager of unincorporated enterprise - employees not stated`) %>%
  select(sa2_name,
         `Employee not owning business` = Employee,
         `Owner managers of incorporated enterprises`,
         `Owner managers of unincorporated enterprises`,
         `Contributing family workers` = `Contributing family worker`,
         year) %>%
  pivot_longer(cols = c(-sa2_name, -year),
               names_to = "employment_status",
               values_to = "employment")

sa2_siemp_2011 <- sa2_pow_siemp("data-raw/sa2_pow_siemp_2011.csv",
                                skip = 9,
                                n_max = 15645,
                                year = 2011)

usethis::use_data(sa2_siemp_2016, compress = "xz", overwrite = TRUE)
usethis::use_data(sa2_siemp_2011, compress = "xz", overwrite = TRUE)




