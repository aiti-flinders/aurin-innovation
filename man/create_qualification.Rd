% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/human_knowledge.R
\name{create_qualification}
\alias{create_qualification}
\title{Create Regional Qualification Data}
\usage{
create_qualification(year = 2016, geography = "sa2")
}
\arguments{
\item{year}{numeric. The year to create the regional qualification data. Must be a census year (2011, 2016).}

\item{geography}{string. The geography level to create the regional qualification data. Defaults to SA2.}
}
\value{
A data frame of the average level of qualification by a geographic area for a given year.
}
\description{
\code{create_qualification()} creates a data frame with the average level of qualification of the people who work in a geographic
area for a given year.
}
\details{
Data is sourced from the ABS Census of Population and Housing - QALLP

Non-school qualifications, based on level of education describes the level of a person's highest completed non-school qualification.
In the Census, it counts the number of persons aged 15 years and over who stated a completed qualification. The education levels are:
\itemize{
\item{Postgraduate Degree Level}
\item{Graduate Diploma and Graduate Certificate Level}
\item{Bachelor Degree Level}
\item{Advanced Diploma and Diploma Level}
\item{Certificate Level}
}

A region is scored based on the proportion of workers within each qualification level, where
the Graduate Diploma and Graduate Certificate Level is combined with the Bachelor Degree Level. As such,
each region receives a score between 1 and 4, where higher scores indicate a higher average level of education.
}
\examples{
create_qualification(2016)
}
