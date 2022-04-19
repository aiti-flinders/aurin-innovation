create_kibs <- function(year = NULL, geography = NULL, adjust = TRUE) {

  if (adjust) {

    data <- sa2_indp4_2016 %>%
      adjust_nfd(nfd_col = "anzsic_class",
                 nfd_level = "anzsic_class",
                 anz = "anzsic")

  } else {

    data <- sa2_indp4_2016

  }

  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

  } else {
      geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
        sf::st_drop_geometry()
    }

  employment_kibs <- data %>%
    dplyr::group_by(kibs = anzsic_class %in% kibs(),
                    sa2_name) %>%
    dplyr::mutate(employment = sum(employment)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(sa2_name, employment, kibs) %>%
    tidyr::pivot_wider(names_from = kibs,
                       names_prefix = "kibs",
                       values_from = employment)  %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(dplyr::across(c(kibsFALSE, kibsTRUE), ~sum(.x)), .groups = "drop") %>%
    dplyr::mutate(total_employment = kibsTRUE + kibsFALSE) %>%
    dplyr::rename(non_kibs = kibsFALSE,
                  kibs = kibsTRUE) %>%
    dplyr::select(-total_employment)



  return(employment_kibs)


}
