#' Create Regional Qualification Data
#'
#' `create_qualification()` creates a data frame with the average level of qualification of the people who work in a geographic
#' area for a given year.
#'
#' Data is sourced from the ABS Census of Population and Housing - QALLP
#'
#' Non-school qualifications, based on level of education describes the level of a person's highest completed non-school qualification.
#' In the Census, it counts the number of persons aged 15 years and over who stated a completed qualification. The education levels are:
#' \itemize{
#' \item{Postgraduate Degree Level}
#' \item{Graduate Diploma and Graduate Certificate Level}
#' \item{Bachelor Degree Level}
#' \item{Advanced Diploma and Diploma Level}
#' \item{Certificate Level}
#' }
#'
#' A region is scored based on the proportion of workers within each qualification level, where
#' the Graduate Diploma and Graduate Certificate Level is combined with the Bachelor Degree Level. As such,
#' each region receives a score between 1 and 4, where higher scores indicate a higher average level of education.
#'
#' @param year numeric. The year to create the regional qualification data. Must be a census year (2011, 2016).
#' @param geography string. The geography level to create the regional qualification data. Defaults to SA2.
#'
#' @return A data frame of the average level of qualification by a geographic area for a given year.
#' @export
#'
#' @examples
#' create_qualification(2016)
create_qualification <- function(year = 2016, geography = "sa2") {

  stopifnot("Year must be one of 2011, 2016" = year %in% c(2011, 2016))
  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (year == 2016) {

    geog <- sa2_2016

    data <- sa2_qallp1_2016

  } else {

    geog <- sa2_2011

    data <- sa2_qallp1_2011
  }


  data %>%
    dplyr::left_join(geog, by = geography) %>%
    dplyr::group_by(.data[[geography]], .data$education_level) %>%
    dplyr::summarise(employment = sum(.data$employment), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$education_level,
                       values_from = .data$employment) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dip = .data$advanced_diploma_and_diploma_level,
                  cert = .data$certificate_level,
                  grad = .data$bachelor_degree_level + .data$graduate_diploma_and_graduate_certificate_level,
                  postgrad = .data$postgraduate_degree_level,
                  qualification = (4 * .data$postgrad + 3 * .data$grad + 2 * .data$dip + 1 * .data$cert) / (.data$postgrad + .data$grad + .data$dip + .data$cert)) %>%
    dplyr::select(.data[[geography]],
                  .data$qualification)



}

#' Create Regional Stem Employment Data.
#'
#' `create_stem()` creates a data frame with the proportion of workers in a geographic area who have a qualification in a STEM degree.
#'
#' Data is sourced from the ABS Census of Population and Housing - QALFP.
#'
#' Non-school qualifications, based on field of study describes the field of study of a person's highest completed non-school qualification.
#' In the Census, it counts the number of persons aged 15 years and over who stated a completed qualification.
#'
#' Fields of study are defined using the 2001 version of the Australian Standard Classification of Education (ASCED).
#'
#' STEM qualifications refer to any non-school qualifications in any of the following fields:
#' \itemize{
#' \item{Natural and Physical Sciences}
#' \item{Information Technology}
#' \item{Engineering and Related Technologies}
#' \item{Agriculture, Environmental and Related Studies}
#' }
#'
#' @param year numeric. The year to create the regional stem employment data. Must be a census year (2011, 2016).
#' @param geography string. The geography level to create the regional stem employment data. Defaults to SA2.
#'
#' @return A data frame containing the proportion of workers in a geographic area who have a qualification in a STEM degree.
#' @export
#'
#' @examples
#' create_stem(2016, "SA2")
create_stem <- function(year = 2016, geography = "sa2") {

  stopifnot("Year must be one of 2011, 2016" = year %in% c(2011, 2016))
  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  if (year == 2016) {

    geog <- sa2_2016

    data <- sa2_qalfp2_2016

  } else {

    geog <- sa2_2011
    data <- sa2_qalfp2_2011
  }


  data %>%
    dplyr::left_join(geog, by = geography) %>%
    dplyr::group_by(stem = .data$qualification %in% c("Natural and Physical Sciences",
                                         "Information Technology",
                                         "Engineering and Related Technologies",
                                         "Agriculture, Environmental and Related Studies"),
                    .data[[geography]]) %>%
    dplyr::summarise(stem_employees = sum(.data$employment), .groups = "drop")  %>%
    tidyr::pivot_wider(names_from = .data$stem, values_from = .data$stem_employees, names_prefix = "stem") %>%
    dplyr::mutate(stem = .data$stemTRUE/(.data$stemFALSE + .data$stemTRUE),
                  year = {{year}}) %>%
    dplyr::select(geography,
                  .data$stem,
                  .data$year)

}

create_human_knowledge <- function(year, geography = "sa2") {

  join_by <- paste0(tolower(geography), "_name")

  dplyr::full_join(create_qualification(year, geography),
                   create_stem(year, geography), join_by)
}
