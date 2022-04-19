create_owner_managers <- function(year, geography) {


  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_siemp_2016


  } else {
    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_siemp_2011
  }

  data %>%
    tidyr::pivot_wider(names_from = employment_status,
                       values_from = employment) %>%
    dplyr::mutate(total = rowSums(dplyr::across(c(2:9))),
                  owners = rowSums(dplyr::across(dplyr::contains("Owner manager")))) %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(dplyr::across(c(3:12), ~sum(.x))) %>%
    dplyr::mutate(owner_share = owners/total) %>%
    dplyr::select(geography,
                  owner_share)




}
