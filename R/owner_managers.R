#' Create Regional Owner Manager Data
#'
#' `create_owner_managers()` creates a data frame with the proportion of workers in a geographic area who are
#' owner managers of incorporated and unincorporated enterprises.
#'
#' Data is sourced from the ABS Census of Population and Housing - SIEMP and the ABS Census of Population and Housing - EMTP
#'
#' Status in employment defines an employed persons status in employment for the main job held in the week prior to Census night.
#' The 2016 Census replaced the employment type (EMTP) measure with the status in employment measure (SIEMP). For consistency, the
#' 2016 SIEMP data is aggregated to match the employment types present in the 2011 Census which were:
#' \itemize{
#' \item{Employee not owning business}
#' \item{Owner managers of incorporated enterprises}
#' \item{Owner managers of unincorporated enterprises}
#' \item{Contributing family workers}
#' }
#'
#' @param year numeric. The year to create the regional owner manager data. Must be a census year (2011, 2016).
#' @param geography string. The geography level to create the regional owner manager data. Defaults to SA2.
#'
#' @return A data frame of the proportion of workers in a geographic area who are owner managers of incorporated and
#' unincorporated enterprises.
#' @export
#'
#' @examples
#' create_owner_managers(2016)
create_owner_managers <- function(year = 2016, geography = "sa2") {

  stopifnot("Year must be one of 2011, 2016, or 2021" = year %in% c(2011, 2016, 2021))
  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (year == 2021) {
    geog <- sa2_2021
    data <- sa2_siemp_2021
  }
  if (year == 2016) {

    geog <- sa2_2016

    data <- sa2_siemp_2016


  } else if (year == 2011) {

    geog <- sa2_2011

    data <- sa2_siemp_2011
  }

  data %>%
    tidyr::pivot_wider(names_from = .data$employment_status,
                       values_from = .data$employment) %>%
    dplyr::mutate(total = rowSums(dplyr::across(c(-.data$sa2_name, -.data$year))),
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
    dplyr::mutate(owner_managers = .data$owners,#/.data$total,
                  year = {{year}}) %>%
    dplyr::select(geography,
                  .data$owner_managers,
                  .data$year)




}
