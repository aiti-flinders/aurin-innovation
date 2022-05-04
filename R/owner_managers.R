create_owner_managers <- function(year, geography) {


  if (year == 2016) {

    geog <- strayr::read_absmap("sa22016", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_siemp_2016


  } else if (year == 2011) {

    geog <- strayr::read_absmap("sa22011", remove_year_suffix = TRUE) %>%
      sf::st_drop_geometry()

    data <- sa2_siemp_2011
  }

  data %>%
    tidyr::pivot_wider(names_from = employment_status,
                       values_from = employment) %>%
    dplyr::mutate(total = rowSums(dplyr::across(c(-sa2_name, -year))),
                  owners = rowSums(dplyr::across(dplyr::contains("Owner manager")))) %>%
    dplyr::left_join(geog, by = "sa2_name") %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(dplyr::across(c("Employee not owning business",
                                     "Owner managers of incorporated enterprises",
                                     "Owner managers of unincorporated enterprises",
                                     "Contributing family workers",
                                     "total",
                                     "owners"),
                                   ~sum(.x)),
                     .groups = "drop") %>%
    dplyr::mutate(owner_managers = owners/total,
                  year = {{year}}) %>%
    dplyr::select(geography,
                  owner_managers,
                  year)




}
