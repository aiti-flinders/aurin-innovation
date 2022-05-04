create_graduates <- function(year = 2016, geography = "sa3_name") {

  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_qallp1_2016

  } else {

    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_qallp1_2011
  }


  data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], education_level) %>%
    dplyr::summarise(employment = sum(employment), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = education_level,
                       values_from = employment) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dip = advanced_diploma_and_diploma_level,
                  cert = certificate_level,
                  grad = bachelor_degree_level + graduate_diploma_and_graduate_certificate_level,
                  postgrad = postgraduate_degree_level,
                  rank = (4 * postgrad + 3 * grad + 2 * dip + 1 * cert) / (postgrad + grad + dip + cert)) %>%
    dplyr::select(.data[[geography]],
                  rank)



}

create_stem <- function(year, geography) {

  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_qalfp2_2016

  } else {

    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_qalfp2_2011
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
    dplyr::mutate(stem = stemTRUE/(stemFALSE + stemTRUE),
                  year = {{year}}) %>%
    dplyr::select(geography, stem, year)

}

create_human_knowledge <- function(year, geography) {
  dplyr::full_join(create_graduates(year, geography),
                   create_stem(year, geography))
}
