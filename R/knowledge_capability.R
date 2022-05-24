create_knowledge_capability <- function(year, geography) {

  # Specify only to supress message.
  join_by <- c(paste0(tolower(geography), "_name"), "year")

  knowledge_capability <- dplyr::full_join(create_patents(year, geography),
                                           create_designs(year, geography), by = join_by) %>%
    dplyr::full_join(create_trademarks(year, geography), by = join_by) %>%
    dplyr::full_join(create_plants(year, geography), by = join_by)

  knowledge_capability

}
