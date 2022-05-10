#' Create Regional Skill Data
#'
#' `create_skill()` creates a data frame of the average skill level of the occupations of workers in a geographic area for a given year.
#'
#' The Australian Skills Commission provides data on the core competencies of occupations as defined by the 2006 ANZSCO at the unit level.
#' Each occupation is scored across 10 core competencies, receiving a score between 1 and 10 where 10 is the most skilled.
#'
#' The skill level of a region is defined as the weighted average of an occupations score and the share of employment
#' in that occupation in a geographic area. Skill level can range from 1 to 10, where 10 is the most skilled.
#'
#' @param year numeric. The year to create the regional skill data. Must be a census year (2011, 2016).
#' @param geography string. The geography level to create the regional skill data. Defaults to SA2.
#' @param adjust logical. TRUE to remove industries classified as "not further defined"
#'
#' @return A data frame of the average skill of a geographic area for a given year.
#' @export
#' @importFrom stats weighted.mean
#'
#' @examples create_skill(2016)
create_skill <- function(year, geography = "sa2", adjust = FALSE) {

  stopifnot("Year must be one of 2011, 2016" = year %in% c(2011, 2016))
  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")


  if (year == 2016) {

    geog <- sa2_2016
    data <- sa2_occp4_2016

  } else  {
    geog <- sa2_2011
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
    dplyr::left_join(occp_skill, by = "anzsco_name") %>%
    dplyr::left_join(geog, by = geography) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::mutate(employment_share = .data[[emp]] / sum(.data[[emp]])) %>%
    dplyr::summarise(skill = weighted.mean(x = score, w = employment_share, na.rm = T))

  return(skill_level)

}
