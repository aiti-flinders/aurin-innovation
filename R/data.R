#' Innovation metrics of SA2 regions for 2011 and 2016.
#'
#' A dataset containing a measure of innovation, inputs to innovation, and other innovation
#' metrics for SA2 regions.
#'
#' @format A data frame with 4325 rows and 20 variables:
#' \describe{
#' \item{sa2_name}{Name of the SA2 region. For 2011, uses the 2011 Australian Statistical Geography Standard.
#' For 2016, uses the 2016 Australian Statistical Geography Standard}
#' \item{sa2_code}{8-digit code assigned to the sa2 name}
#' \item{year}{}
#' \item{patents}{Number of patents per 1000 workers. Patent data is derived from the 2019
#' Intellectual Property Government Open Data (IPGOD) tables 101 and 102.
#' All patent applications from a valid SA2 are included.}
#' \item{backwards_citations}{Number of backward citations. Patent data is derived from the 2019
#' IPGOD tables 110.}
#' \item{designs}{Number of designs per 1000 workers. Designs data is derived from the 2019 IPGOD
#' tables 301 and 302. Only designs whose application originated from a valid SA2 in Australia are included. }
#' \item{trademarks}{Number of trademarks per 1000 workers. Trademarks data is derived from the 2019 IPGOD
#' tables 201 and 201. Only trademarks whose application originated from a valid SA2 in Australia are included.}
#' \item{plants}{Number of plant breeder rights applications per 1000 workers. Plant Breeder Rights data
#' is derived from the 2019 IPGOD tables 401 and 402. Only plant breeder rights lodged from a valid SA2 in Australia are
#' included. }
#' \item{kibs}{Proportion of workers in knowledge intensive business services. See `?kibs_industry_class`.}
#' \item{skill}{Average skill level of region occupations. Occupation skill levels are derived from the
#' Australian Skills Commission Core Competencies of occupations as defined by the 2006 ANZSCO at the unit level. Each
#' occupation is scored across 10 competencies, receiving a score between 1 and 10 where 10 is the most skilled. The
#' skill level of a region is the weighted average of an occupations skill level, and the share of employment in that
#' occupation in an SA2.  Skill ranges from 1 to 10 where 10 is the most skilled. }
#' \item{owner_managers}{Proportion of workers who are owner managers of incorporated and unincorporated enterprises.
#' Data is derived from the ABS Census of Population and Housing SIEMP for 2016 and EMTP for 2011.}
#' \item{unis}{Number of universities located within SA2 boundaries. University point data is sourced from the
#' \href{https://data.aurin.org.au/dataset/aurin-aurin-national-education-dataset-universities-2018-na}{AURIN Portal.}}
#' \item{tafes}{Number of TAFEs located within SA2 boundaries. TAFE point data is sourced from the
#' \href{https://data.aurin.org.au/dataset/aurin-aurin-national-education-dataset-tafes-2018-na}{AURIN Portal.}}
#' \item{qualification}{Average education level of those employed in the region. Data is sourced from the ABS
#' Census of Population and Housing QALLP for 2011 and 2016. A region is scored based on the proportion of workers within
#' each education qualification level, where Certificate level qualifications are worth 1 and Postgraduate Degree level qualifications
#' are worth 4. The Graduate Diploma and Graduate Certificate level is combined with the Bachelor Degree level. As such,
#' each region receives a score between 1 and 4, where a higher score indicates a higher average level of education. }
#' \item{stem}{Proportion of workers with a qualification in a stem field. STEM qualifications refer to any non-school qualifications
#' in Natural and Physical Sciences, Information Technology, Engineering and Related Technologies, and Agriculture, Environmental and Related Studies}
#' \item{employment}{Total employment, measured by the ABS 2011 and 2016 Census, Counting Place of Work.}
#' \item{infrastructure}{Number of universities and tafes}
#' \item{human_knowledge}{Factor derived from kibs, skill, and qualification}
#' \item{patent_output}{Factor derived from patents and backwards citations}
#' \item{innovation}{Second-order factor derived from human_knowledge and patent_output}
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

#' Knowledge intensive business services industries by ANZSIC class.
#'
#' A list of the 68 ANZSIC industry classes considered to be knowledge intensive businesses services (KIBS).
#' Knowledge Intensive Business Services are defined based on the list provided in the New Zealand Government
#' \href{https://www.mbie.govt.nz/assets/bd287dd4f2/Knowledge-intensive-services-report.pdf}{Knowledge Intensive Services Report}
#'
"kibs_industry_class"
