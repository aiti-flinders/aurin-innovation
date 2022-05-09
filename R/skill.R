create_skill <- function(year, geography, adjust = TRUE) {

  if (year == 2016) {

    geog <- sa2_2016 %>%
      sf::st_drop_geometry()

    data <- sa2_occp4_2016

  } else if (year == 2011)  {
    geog <- sa2_2011 %>%
      sf::st_drop_geometry()

    data <- sa2_occp4_2011
  }

  if (adjust) {
    data <- data %>%
      adjust_nfd(nfd_col = "anzsco_name",
                 nfd_level = "anzsco_unit",
                 anz = "anzsco")

    emp <- "employment_adj"
  } else {

    data <- data
    emp <- "employment"
  }

  occp_skill <- strayr::asc_core_competencies %>%
    dplyr::group_by(anzsco_name) %>%
    dplyr::summarise(score = mean(score))

  skill_level <- data %>%
    dplyr::left_join(occp_skill) %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::mutate(employment_share = .data[[emp]] / sum(.data[[emp]])) %>%
    dplyr::summarise(skill = weighted.mean(x = score, w = employment_share, na.rm = T))

  return(skill_level)

}
