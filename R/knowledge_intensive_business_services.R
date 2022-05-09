create_kibs <- function(year = NULL, geography = NULL, adjust = TRUE) {


  if (year == 2016) {

    geog <- sa2_2016 %>%
      sf::st_drop_geometry()

    data <- sa2_indp4_2016


  } else if (year == 2011) {
      geog <- sa2_2011 %>%
        sf::st_drop_geometry()

      data <- sa2_indp4_2011
  }

  if (adjust) {

    data <- data %>%
      adjust_nfd(nfd_col = "anzsic_class",
                 nfd_level = "anzsic_class",
                 anz = "anzsic") %>%
      dplyr::select(sa2_name, anzsic_class, employment = employment_adj)

  } else {

    data <- data

  }

  employment_kibs <- data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(kibs = anzsic_class %in% kibs(),
                    .data[[geography]]) %>%
    dplyr::summarise(kibs_employment = sum(employment), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = kibs, names_prefix = "kibs", values_from = kibs_employment) %>%
    dplyr::mutate(kibs = kibsTRUE / (kibsFALSE + kibsTRUE),
                  year = {{year}}) %>%
    dplyr::select(geography, kibs, year)



  return(employment_kibs)


}
