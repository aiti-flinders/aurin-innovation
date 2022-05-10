create_infrastructure <- function(year, geography = "sa3_name") {

  if (year == 2011) {

    geog <- sa2_2011 %>%
      sf::st_drop_geometry()
    data <- education_location

  } else if (year == 2016) {

    geog <- sa2_2016 %>%
      sf::st_drop_geometry()
    data <- education_location
  }

  data %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(dplyr::across(c(unis, tafes), ~sum(.x, na.rm = TRUE))) %>%
    dplyr::ungroup()



}
