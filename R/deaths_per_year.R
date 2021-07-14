
#' @title Estimate average annual deaths between censuses
#'
#' @description Use annual death counts for all or some years between
#'   two censuses to approximate average annual deaths in the
#'   intercensal period. Used to prep input data for death distribution methods.
#'
#' @param years_deaths \[`numeric()`\]\cr
#'   Which years reported deaths correspond to.
#' @param input_deaths \[`numeric()`\]\cr
#'   How many deaths were recorded for each of the years in `years_deaths`.
#' @param years_census \[`numeric(2)`\]\cr
#'   Vector of two numerics, where each number is a census year, and together
#'   they form the bounds of the intercensal period.
#' @param step_size \[`numeric()`\]\cr
#'   Interpolate annual deaths at intervals of `step_size`. Default is 0.001
#'   and annual deaths at intervals of 0.001 years will be approximated with
#'   interpolation.
#' @param rule \[`numeric()`\]\cr
#'   Rule to be passed to `stats::approxfun` for extrapolation.
#'   Default is '2' and constant extrapolation from the extremes is used.
#' @param ... Other arguments to be passed to `stats::approxfun`.
#'
#' @return \[`numeric(1)`\]\cr
#'   Average annual deaths in the period.
#'
#' @details
#' If only one year of death counts is available, that count is returned as the
#' average annual death count. If multiple years are available, linear
#' interpolation is used to approximate annual death counts at all points
#' in the intercensal period, and then a simple average is taken. Constant
#' extrapolation is used at the bounds (`stats::approxfun` rule = 2) by
#' default.
#'
#' @examples
#' annual_deaths <- deaths_per_year(
#'   years_deaths = c(2000, 2005, 2006),
#'   input_deaths = c(100, 110, 114),
#'   years_census = c(2000, 2010)
#' )
#'
#' @export
deaths_per_year <- function(years_deaths,
                            input_deaths,
                            years_census,
                            step_size = 0.001,
                            rule = 2,
                            ...) {

  # check
  checkmate::assert_numeric(years_deaths)
  checkmate::assert_numeric(input_deaths)
  checkmate::assert_true(length(years_deaths) == length(input_deaths))
  checkmate::assert_numeric(years_census, len = 2)
  checkmate::assert_true(years_census[1] < years_census[2])
  checkmate::assert_numeric(step_size, len = 1)

  # if only one death year provided, make that the mean
  if (length(input_deaths) == 1) {
    return (input_deaths)

  # otherwise, continue
  } else {

    # linearly interpolate years, using constant extrapolation
    deaths_from_year <- stats::approxfun(
      x = years_deaths,
      y = input_deaths,
      rule = rule,
      ...
    )

    # find interpolated 1-year deaths during the entire intercensal period
    deaths_all_years <- deaths_from_year(
      seq(years_census[1], years_census[2], step_size)
    )

    # take a simple mean and return
    deaths_per_year <- mean(deaths_all_years[!is.na(deaths_all_years)])
    return(deaths_per_year)

  }
}
