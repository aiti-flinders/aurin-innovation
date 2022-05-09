#' Plant Breeder Rights data
#'
#' Creates a datafrmae with the number of plant breeder rights applied for in a geographic area for a given year.
#' @param year numeric. The year the plant breeder right application was submitted.
#' @param geography string. The geographic region of interest. Defaults to SA2
#'
#' @return A dataframe of plant breeder rights by geography for a given year.
#' @export
#'
#' @examples \dontrun{
#' create_plants(2016, "SA2")
#' }
create_plants <- function(year, geography = NULL) {

  if (!year %in% c(2011, 2016, 2021)) {
    warning("A non-census year was specified.
            Other indicators of innovation are only available for the census years 2011, 2016, and 2021.
            Proceed with caution. ")
  }

  if (year == 2011) {
    geog <- sa2_2011
  } else {
    geog <- sa2_2016
  }

  stopifnot(tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (!file.exists("data-raw/ipgod402.csv")) {

    download.file("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/43d22a58-b182-4e24-97b4-7a68dd0f9add/download/ipgod402-update.csv",
                  destfile = "data-raw/ipgod402.csv")

    plants <- readr::read_csv("data-raw/ipgod402.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    plants <- readr::read_csv("data-raw/ipgod402.csv") %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod401.csv")) {

    download.file("https://data.gov.au/data/dataset/24a216fc-97e6-41b3-b13b-77c972922003/resource/8968df95-6268-4c3c-8007-23cb3de1066a/download/ipgod401.csv",
                  destfile = "data-raw/ipgod401.csv")

    plants_info <- readr::read_csv("data-raw/ipgod401.csv") %>%
      dplyr::rename(application_year = appl_received_year)

  } else {

    plants_info <- readr::read_csv("data-raw/ipgod401.csv") %>%
      dplyr::rename(application_year = appl_received_year)
  }

  p <- plants %>%
    dplyr::left_join(plants_info) %>%
    dplyr::filter(year == {{year}},
                  !is.na(sa2_name)) %>%
    dplyr::group_by(sa2_name, year) %>%
    dplyr::summarise(plants = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup()

  if (year < 2016) {

    p <- p %>% sa2_16_to_sa2_11(var = "plants")
  }

  p %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(plants = sum(plants)) %>%
    dplyr::ungroup()

}
