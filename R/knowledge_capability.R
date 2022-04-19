create_knowledge_capability <- function(year, geography) {

  knowledge_capability <- dplyr::left_join(
    x = purrr::map_dfr(.x = year, .f = ~create_patents(.x, {{geography}})),
    y = purrr::map_dfr(.x = year, .f = ~create_designs(.x, {{geography}}))
  ) %>%
    dplyr::left_join(
      y = purrr::map_dfr(.x = year, .f = ~create_trademarks(.x, {{geography}})),
      by = c({{geography}}, "year")
      ) %>%
    dplyr::left_join(
      y = purrr::map_dfr(.x = year, .f = ~create_plants(.x, {{geography}})),
      by = c({{geography}}, "year")
    ) %>%
    tidyr::replace_na(list(patents = 0,
                           designs = 0,
                           trademarks = 0,
                           plants = 0))

  return(knowledge_capability)

}
