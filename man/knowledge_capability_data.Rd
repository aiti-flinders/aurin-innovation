% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{knowledge_capability_data}
\alias{knowledge_capability_data}
\title{Knowledge capability metrics of SA2 regions between 2011 and 2016.}
\format{
A data frame with 14122 rows and 9 variables:
\describe{
\item{sa2_name}{name of SA2 region. For years before 2016, uses the 2011 Australian Statistical Geography Standard (ASGS).  For years following and including 2016, uses the 2016 ASGS. More information about the ASGS can be read on the Australian Bureau of Statistics (ABS) website \href{https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026}{here}.}
\item{sa2_code}{8 digit code representing the SA2 region.}
\item{year}{}
\item{patents}{number of patent applications received from within the SA2 for the given year.}
\item{backwards_citations}{ total number of backwards citations for all patent applications within the SA2 for the given year.}
\item{designs}{number of design applications received from within the SA2 for the given year.}
\item{trademarks}{number of trademark applications received from within the SA2 for the given year.}
\item{plants}{number of plant breeder rights applications received from within the SA2 for the given year.}
\item{employment}{total number of people whose place of work is within the SA2. Data is derived from the ABS Census. For years before 2016, uses place of work employment data from the 2011 Census. For years following and including 2016, uses place of work employment data from the 2016 Census.}
}
}
\usage{
knowledge_capability_data
}
\description{
A dataset containing the number of patents, designs, trademarks, and plant breeder rights
occuring in SA2 regions.
}
\keyword{datasets}
