#' Innovation metrics of SA2 regions for 2011 and 2016.
#'
#' A dataset containing a measure of innovation, inputs to innovation, and other innovation
#' metrics for SA2 regions.
#'
#' @format A data frame with 4325 and 19 variables:
#' \describe{
#' \item{sa2_name}
#' \item{year}
#' \item{patents}{number of patents per 1000 workers, z score}
#' \item{backwards_citations}{average number of backward citations, z score}
#' \item{designs}{number of designs per 1000 workers, z score}
#' \item{trademarks}{number of trademarks per 1000 workers, z score}
#' \item{plants}{number of plant breeder rights applications per 1000 workers, z score}
#' \item{kibs}{proportion of workers in knowledge intensive business services, z score}
#' \item{skill}{average skill level of occupation, z score}
#' \item{owner_managers}{proportion of owner-managers, z score}
#' \item{unis}{number of universities, z score}
#' \item{tafes}{number of tafes, z score}
#' \item{qualification}{average education level of workers, z score}
#' \item{stem}{proportion of workers with a stem qualification, zs core}
#' \item{employment}{total employment, z score}
#' \item{infrastructure}{number of universities and tafes, z score}
#' \item{human_knowledge}{factor derived from kibs, skill, and qualification}
#' \item{patent_output}{factor derived from patents and backwards citations}
#' \item{innovation}{second-order factor derived from human_knowledge and patent_output}
#' }
#'
#
"regional_innovation"
