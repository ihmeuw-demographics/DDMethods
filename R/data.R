
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
