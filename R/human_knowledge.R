create_graduates <- function(year = 2016, geography = "sa3_name") {

  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_heap1_2016

  } else {

    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_heap1_2011
  }


  data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], education_level) %>%
    dplyr::summarise(employment = sum(employment), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = education_level,
                       values_from = employment) %>%
    dplyr::mutate(university = `Bachelor Degree Level` + `Postgraduate Degree Level`,
                  tafe = `Certificate III & IV Level` + `Certificate I & II Level` + `Advanced Diploma and Diploma Level`,
                  school = `Secondary Education - Years 10 and above`,
                  no_school = `Secondary Education - Years 9 and below`) %>%
    dplyr::select(geography,
                  university,
                  tafe,
                  school,
                  no_school) %>%
    dplyr::mutate(total = university + tafe + school + no_school,
                  dplyr::across(c(university, tafe, school, no_school), ~.x / total)) %>%
    dplyr::select(-total)


}

create_stem <- function(year, geography) {

  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_qalfp2_2016

  } else {

    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_qalfp1_2011
  }


  data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(stem = qualification %in% c("Natural and Physical Sciences",
                                         "Information Technology",
                                         "Engineering and Related Technologies",
                                         "Agriculture, Environmental and Related Studies"),
                    .data[[geography]]) %>%
    dplyr::summarise(stem_employees = sum(employment), .groups = "drop")  %>%
    tidyr::pivot_wider(names_from = stem, values_from = stem_employees, names_prefix = "stem") %>%
    dplyr::mutate(stem_share = stemTRUE/(stemFALSE + stemTRUE)) %>%
    dplyr::select(geography, stem_share)

}

create_human_knowledge <- function(year, geography) {
  dplyr::full_join(create_graduates(year, geography),
                   create_stem(year, geography))
}
