create_skill <- function(year, geography, adjust = TRUE) {

  if (adjust) {
    data <- sa2_occp4_2016 %>%
      adjust_nfd(nfd_col = "anzsco_name",
                 nfd_level = "anzsco_unit",
                 anz = "anzsco")

    emp <- "employment_adj"
  } else {

    data <- sa2_occp4_2016
    emp <- "employment"
  }

  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

  } else {
    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()
  }

  skill_level <- data %>%
    dplyr::left_join(strayr::asc_core_competencies, by = "anzsco_name") %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::mutate(employment_share = .data[[emp]] / sum(.data[[emp]])) %>%
    dplyr::summarise(skill = sum(employment_share * score, na.rm = T))

  return(skill_level)

}
