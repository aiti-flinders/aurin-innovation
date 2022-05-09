#' Designs data
#'
#' Creates a dataframe with the number of designs files in a geographic are for a given year.
#'
#' @param year numeric. The year the designs were filed.
#' @param geography string. The geographic region of interest. Defaults to SA2
#'
#' @return A dataframe of designs by geography for a given year.
#' @export
#'
#' @examples \dontrun{
#' create_designs(2016, "SA2")
#' }
create_designs <- function(year, geography = "sa2") {

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


  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")


  if (!file.exists("data-raw/ipgod302.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/4b802e80-c667-4b84-8f50-72c2624c59c1/download/ipgod302.csv",
                  destfile = "data-raw/ipgod302.csv")

    designs <- readr::read_csv("data-raw/ipgod302.csv",
                               show_col_types = FALSE) %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))

  }  else {

    designs <- readr::read_csv("data-raw/ipgod302.csv",
                               show_col_types = FALSE) %>%
      dplyr::mutate(sa2_code = as.character(sa2_code))
  }

  if (!file.exists("data-raw/ipgod301.csv")) {

    download.file("https://data.gov.au/data/dataset/a4210de2-9cbb-4d43-848d-46138fefd271/resource/71770a53-8727-4f00-a423-db4878f910f6/download/ipgod301.csv",
                  destfile = "data-raw/ipgod301.csv")

    designs_info <- readr::read_csv("data-raw/ipgod301.csv",
                                    show_col_types = FALSE) %>%
      dplyr::rename(year = filing_year)

  } else {

    designs_info <- readr::read_csv("data-raw/ipgod301.csv",
                                    show_col_types = FALSE) %>%
      dplyr::rename(year = filing_year)
  }


  d <- designs %>%
    dplyr::left_join(designs_info, "application_id", "australian", "entity") %>%
    dplyr::filter(year == {{year}},
                  !is.na(sa2_name)) %>%
    dplyr::group_by(sa2_name, year) %>%
    dplyr::summarise(designs = dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup()

  if (year < 2016) {
    d <- d %>% sa2_16_to_sa2_11(var = "designs")
  }

  d %>%
    dplyr::left_join(geog) %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(designs = sum(designs)) %>%
    dplyr::ungroup()
}
