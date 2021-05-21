
#' South Africa male input data for 2001-2007 period
#'
#' @description Input deaths and population counts for the 2001-2007 period
#'   for males in South Africa. Comes from example here:
#'   http://demographicestimation.iussp.org/content/generalized-growth-balance-method.
#'
#' @format \[`data.table()`\] columns:
#' \describe{
#'  \item{location}{\[`character()`\]\cr "South Africa"}
#'  \item{pop1}{\[`numeric()`\]\cr Age-specific population from census 1}
#'  \item{pop2}{\[`numeric()`\]\cr Age-specific population from census 2}
#'  \item{deaths}{\[`numeric()`\]\cr Age-specific deaths recorded in intercensal period}
#'  \item{migrants}{\[`numeric()`\]\cr Age-specific net migration recorded in intercensal period}
#'  \item{age}{\[`integer()`\]\cr Lower bound of 5-year age group}
#'  \item{sex}{\[`integer()`\]\cr "male"}
#'  \item{date1}{\[`numeric()`\]\cr Date of first census}
#'  \item{date2}{\[`numeric()`\]\cr Date of second census}
#' }
#' @examples
#' zaf_2001_2007
"zaf_2001_2007"


#' Coale Demeny life expectancy
#'
#' @description Life expectancy and ratio of 30d10 to 20d40 from Coale
#'   Demeny life tables, to be used for approximation of open age interval
#'   life expectancy, which is an input to SEG DDM estimation. Life tables
#'   are downloaded using the `demogR` package.
#'
#' @format \[`data.table()`\] columns:
#' \describe{
#'  \item{cd_region}{\[`character()`\]\cr "north", "south", "west", or "east"}
#'  \item{sex}{\[`character()`\]\cr "male", "female", or "all}
#'  \item{index}{\[`numeric()`\]\cr Life table level, based on life expectancy at age 10}
#'  \item{age_start}{\[`numeric()`\]\cr Lower bound of age interval}
#'  \item{ex}{\[`numeric()`\]\cr Life expectancy}
#'  \item{ratio_30d10_20d40}{\[`numeric()`\]\cr Ratio 30d10 to 20d40}
#' }
#' @examples
#' coale_demeny_ex
"coale_demeny_ex"
