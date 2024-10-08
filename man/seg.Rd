% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seg.R
\name{seg}
\alias{seg}
\title{Synthetic Extinct Generations (SEG) DDM}
\usage{
seg(
  dt,
  age_trim_lower = 45,
  age_trim_upper = 90,
  drop_open_interval = T,
  id_cols = c("age_start", "sex"),
  migration = F,
  input_deaths_annual = T,
  input_migrants_annual = T,
  method_numerator_denominator_type = "Nx"
)
}
\arguments{
\item{dt}{[\code{data.table()}]\cr
Input data, must include columns 'pop1', 'pop2', 'deaths', 'age_start',
'sex', 'date1', 'date2', 'cd_region', and all \code{id_cols}. Optional column
'migrants' to be included if \code{migration = T}.}

\item{age_trim_lower}{[\code{numeric(1)}]\cr
age-trim: lower bound of 'age_start' included (inclusive)}

\item{age_trim_upper}{[\code{numeric(1)}]\cr
age-trim: upper bound of 'age_start' included (exclusive)}

\item{drop_open_interval}{[\code{logical(1)}]\cr
Whether to drop the open age interval if it falls within the age trim
bounds.}

\item{id_cols}{[\code{character()}]\cr
Column names that uniquely identify rows of \code{dt}.
Must include 'age_start' and 'sex'. When 'age_start' is removed, these
columns define the groups to calculate completeness for.}

\item{migration}{[\code{logical(1)}]\cr
Whether a 'migrants' column should be used. If migration
is available and of reasonable quality, it improves the DDM estimation.
However, these data are not often available.}

\item{input_deaths_annual}{[\code{logical(1)}]\cr
Whether input deaths are formatted as annual average recorded per year.
Alternative is recorded deaths for the entire period between census 1
and census 2. Default TRUE and assumes deaths are annual. If FALSE,
deaths will be divided by the decimal number of years between 'date1' and
'date2'.}

\item{input_migrants_annual}{[\code{logical(1)}]\cr
Whether input migrants are formatted as annual average recorded per year.
Alternative is recorded net migrants for the entire period between census 1
and census 2. Default TRUE and assumes net migrants are annual. If FALSE,
migrants will be divided by the decimal number of years between 'date1' and
'date2'.}

\item{method_numerator_denominator_type}{[\code{character()}]\cr
Completeness point estimates are population estimated from deaths
divided by population estimated from censuses. This setting dictates
the ages the population belongs to. Must be one of:
\itemize{
\item "Nx" : numerator and denominator are both estimated population
aged x. This is the default and matches the GBD methodology.
\item "nNx" : numerator and denominator are both estimated population
aged x to x+n. This matches Tim Riffe DDM package and IUSSP methods.
}}
}
\value{
[\code{list(2)}]\cr
\itemize{
\item \code{dt} [\code{data.table()}] Input \code{dt} returned with additional variables
computed in SEG estimation, such as birthdays age a, population age
a plus, and synthetic cohort population age a plus.
\item \code{completeness} [\code{data.table()}] Estimated completeness by unique
combination of \code{id_cols}.
}
}
\description{
Estimate completeness of death registration using two
censuses with age-specific population, and average annual age-specific
deaths recorded between the censuses, following the  Synthetic Extinct
Generations method.
}
\details{
Columns of \code{dt}:
\itemize{
\item 'pop1': age-specific population recorded at census 1
\item 'pop2': age-specific population recorded at census 2
\item 'deaths': average annual deaths recorded between census 1 and census 2
\item 'age_start': lower bound of age interval
\item 'sex': 'male', 'female', or 'all', used for matching Coale-Demeny life
tables
\item 'date1': date of first census, format yyyy-mm-dd
\item 'date2': date of second census, format yyyy-mm-dd
\item 'cd_region': 'north', 'south', 'east', or 'west', for the Coale-Demeny
region to use for approximation of open age interval life expectancy.
Suggest to default to 'west'.
\item 'migrants' (optional): average annual net migrants between census 1 and
census 2
}
}
\examples{
library(data.table)
dt <- copy(zaf_2001_2007)
dt[, cd_region := "west"]
id_cols <- c("location", "sex", "age_start")
age_trim_lower = 25
age_trim_upper = 65
results <- seg(
  dt,
  age_trim_lower = age_trim_lower,
  age_trim_upper = age_trim_upper,
  id_cols = id_cols,
  migration = FALSE,
  input_deaths_annual = FALSE,
  input_migrants_annual = FALSE
)

}
\references{
Methods are based on the following sources:
\itemize{
\item Bennett NG, Horiuchi S. Mortality estimation from registered deaths in
less developed countries. Demography. 1984 May;21(2):217-33.
\item Dorrington RE. 2013. "The Generalized Growth Balance Method".
In Moultrie TA, RE Dorrington, AG Hill, K Hill, IM Timæus and B Zaba (eds).
Tools for Demographic Estimation. Paris: International Union for the
Scientific Study of Population.
http://demographicestimation.iussp.org/content/generalized-growth-balance-method.
Accessed May 27 2021.
\item Dorrington RE. 2013. "Synthetic extinct generations methods".
In Moultrie TA, RE Dorrington, AG Hill, K Hill, IM Timæus and B Zaba (eds).
Tools for Demographic Estimation. Paris: International Union for the
Scientific Study of Population.
http://demographicestimation.iussp.org/content/synthetic-extinct-generations-methods.
Accessed May 27 2021.
\item Hill K, You D, Choi Y. Death distribution methods for estimating adult
mortality: sensitivity analysis with simulated data errors. Demographic
Research. 2009 Jul 1;21:235-54.
\item Murray CJ, Rajaratnam JK, Marcus J, Laakso T, Lopez AD. What can we
conclude from death registration? Improved methods for evaluating
completeness. PLoS Med. 2010 Apr 13;7(4):e1000262.
\item Tim Riffe, Everton Lima, and Bernardo Queiroz (2017). DDM: Death
Registration Coverage Estimation. R package version 1.0.0.
https://CRAN.R-project.org/package=DDM
}
}
