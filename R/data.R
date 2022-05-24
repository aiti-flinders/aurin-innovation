#' Innovation metrics of SA2 regions for 2011 and 2016.
#'
#' A dataset containing a measure of innovation, inputs to innovation, and other innovation
#' metrics for SA2 regions.
#'
#' @format A data frame with 4325 rows and 19 variables:
#' \describe{
#' \item{sa2_name}{}
#' \item{year}{}
#' \item{patents}{number of patents per 1000 workers}
#' \item{backwards_citations}{number of backward citations}
#' \item{designs}{number of designs per 1000 workers}
#' \item{trademarks}{number of trademarks per 1000 workers}
#' \item{plants}{number of plant breeder rights applications per 1000 workers}
#' \item{kibs}{proportion of workers in knowledge intensive business services}
#' \item{skill}{average skill level of region occupations}
#' \item{owner_managers}{proportion of owner-managers}
#' \item{unis}{number of universities}
#' \item{tafes}{number of tafes}
#' \item{qualification}{average education level of workers}
#' \item{stem}{proportion of workers with a stem qualification}
#' \item{employment}{total employment}
#' \item{infrastructure}{number of universities and tafes}
#' \item{human_knowledge}{factor derived from kibs, skill, and qualification}
#' \item{patent_output}{factor derived from patents and backwards citations}
#' \item{innovation}{second-order factor derived from human_knowledge and patent_output}
#' }
#'
#
"regional_innovation"

#' Knowledge capability metrics of SA2 regions between 2011 and 2016.
#'
#' A dataset containing the number of patents, designs, trademarks, and plant breeder rights
#' occuring in SA2 regions.
#'
#' @format A data frame with 14122 rows and 9 variables:
#' \describe{
#' \item{sa2_name}{name of SA2 region. For years before 2016, uses the 2011 Australian Statistical Geography Standard (ASGS).  For years following and including 2016, uses the 2016 ASGS. More information about the ASGS can be read on the Australian Bureau of Statistics (ABS) website \href{https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026}{here}.}
#' \item{sa2_code}{8 digit code representing the SA2 region.}
#' \item{year}{}
#' \item{patents}{number of patent applications received from within the SA2 for the given year.}
#' \item{backwards_citations}{ total number of backwards citations for all patent applications within the SA2 for the given year.}
#' \item{designs}{number of design applications received from within the SA2 for the given year.}
#' \item{trademarks}{number of trademark applications received from within the SA2 for the given year.}
#' \item{plants}{number of plant breeder rights applications received from within the SA2 for the given year.}
#' \item{employment}{total number of people whose place of work is within the SA2. Data is derived from the ABS Census. For years before 2016, uses place of work employment data from the 2011 Census. For years following and including 2016, uses place of work employment data from the 2016 Census.}
#' }
"knowledge_capability_data"
