create_business_brains <- function(year, geography = "sa2", adjust = FALSE) {

  join_by <- c(paste0(tolower(geography),"_name"), "year")

  business_brains <- dplyr::full_join(create_kibs(year, geography, adjust),
                                      create_skill(year, geography, adjust),
                                      by = join_by) %>%
    dplyr::full_join(create_owner_managers(year, geography), by = join_by)

  return(business_brains)


}

