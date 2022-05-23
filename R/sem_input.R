sem_data <- function(year, geography = "sa2", adjust = FALSE) {

  join_by <- c(paste0(tolower(geography), "_name"), "year")

  kc <- create_knowledge_capability(year, geography)
  bb <- create_business_brains(year, geography, adjust)
  infrastructure <- create_infrastructure(year, geography)
  hk <- create_human_knowledge(year, geography)


  dplyr::full_join(kc, bb, by = join_by) %>%
    dplyr::full_join(infrastructure, by = join_by) %>%
    dplyr::full_join(hk, by = join_by) %>%
    tidyr::replace_na(list(patents = 0,
                           backwards_citations = 0,
                           designs = 0,
                           trademarks = 0,
                           plants = 0,
                           unis = 0,
                           tafes = 0))
}



