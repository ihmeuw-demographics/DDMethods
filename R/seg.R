
#' @title Synthetic Extinct Generations (SEG) DDM
#'
#' @description Estimate completeness of death registration using two
#'   censuses with age-specific population, and average annual age-specific
#'   deaths recorded between the censuses, following the  Synthetic Extinct
#'   Generations method.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data, must include columns 'pop1', 'pop2', 'deaths', 'age_start',
#'   'sex', 'date1', 'date2', 'cd_region', and all `id_cols`. Optional column
#'   'migrants' to be included if `migration = T`.
#' @param age_trim_lower \[`numeric(1)`\]\cr
#'   age-trim lower bound
#' @param age_trim_upper \[`numeric(1)`\]\cr
#'   age trim upper bound
#' @param id_cols \[`character()`\]\cr
#'   Column names that uniquely identify rows of `dt`.
#'   Must include 'age_start' and 'sex'. When 'age_start' is removed, these
#'   columns define the groups to calculate completeness for.
#' @param migration \[`logical(1)`\]\cr
#'   Whether a 'migrants' column should be used. If migration
#'   is available and of reasonable quality, it improves the GGB estimation.
#'   However, these data are not often available.
#'
#' @return \[`list(2)`\]\cr
#'   * \[`data.table()`\] Input `dt` returned with additional variables
#'     computed in SEG estimation, such as birthdays age a, population age
#'     a plus, and synthetic cohort population age a plus.
#'   * \[`data.table()`\] Estimated completeness by unique combination of
#'     `id_cols`.
#'
#' @details
#' Columns of `dt`:
#'   * 'pop1': age-specific population recorded at census 1
#'   * 'pop2': age-specific population recorded at census 2
#'   * 'deaths': average annual deaths recorded between census 1 and census 2
#'   * 'age_start': lower bound of age interval
#'   * 'sex': 'male', 'female', or 'all', used for matching Coale-Demeny life
#'       tables
#'   * 'date1': date of first census, format yyyy-mm-dd
#'   * 'date2': date of second census, format yyyy-mm-dd
#'   * 'cd_region': 'north', 'south', 'east', or 'west', for the Coale-Demeny
#'       region to use for approximation of open age interval life expectancy.
#'       Suggest to default to 'west'.
#'   * 'migrants' (optional): average annual net migrants between census 1 and
#'       census 2
#'
#' @references
#' Methods are based on the following sources:
#'   * http://demographicestimation.iussp.org/content/synthetic-extinct-generations-methods
#'   * Hill K, You D, Choi Y. Death distribution methods for estimating adult
#'     mortality: sensitivity analysis with simulated data errors. Demographic
#'     Research. 2009 Jul 1;21:235-54.
#'     (https://www.demographic-research.org/volumes/vol21/9/default.htm)
#'
#' @examples
#' library(data.table)
#' dt <- copy(zaf_2001_2007)
#' setnames(dt, "age", "age_start")
#' dt[, cd_region := "west"]
#' id_cols <- c("location", "sex", "age_start")
#' age_trim_lower = 25
#' age_trim_upper = 65
#' # convert from total to average annual deaths
#' dt[, deaths := deaths / 5.353425]
#' results <- seg(
#'   dt,
#'   age_trim_lower = age_trim_lower,
#'   age_trim_upper = age_trim_upper,
#'   id_cols = id_cols,
#'   migration = FALSE
#' )
#'
#' @export

seg <- function(dt,
                age_trim_lower = 25,
                age_trim_upper = 65,
                id_cols = c("age_start", "sex"),
                migration = F) {

  # Validate and setup ------------------------------------------------------

  # check types
  checkmate::assert_data_table(dt)
  checkmate::assert_numeric(age_trim_lower, lower = 0, len = 1)
  checkmate::assert_numeric(age_trim_upper, lower = 0, len = 1)
  checkmate::assert_character(id_cols)
  checkmate::assert_logical(migration, len = 1)

  # check columns
  check_vars <- c(id_cols, "pop1", "pop2", "deaths", "date1", "date2", "cd_region")
  if (migration) check_vars <- c(check_vars, "migrants")
  checkmate::assert_names(names(dt), must.include = check_vars)
  checkmate::assert_character(dt$cd_region, pattern = "north|south|west|east")

  # check ID cols
  checkmate::assert_names(id_cols, must.include = c("age_start", "sex"))
  demUtils::assert_is_unique_dt(dt, id_cols)

  # light prep
  dt <- copy(dt)
  id_cols_no_age <- setdiff(id_cols, "age_start")
  setorderv(dt, c(id_cols, "age_start"))


  # Compute components ------------------------------------------------------

  # Denominator: population age a from censuses
  # Approximate with birthdays age a
  gen_bdays_age_a(dt, id_cols_no_age)

  # Numerator: population age a from death registration
  # Estimation in short:
  #  age-specific growth -> cumulative growth -> ndx ->
  #  ratio 30d10/20d40 ->
  #  match with best Coale-Demeny life table level ->
  #  get ex for open-ended interval ->
  #  get synthetic cohort pop starting w/ open-ended interval and working
  #  backwards by age

  # Age-specific growth rate
  gen_age_specific_growth_rate(dt, migration)

  # Cumulative growth rate
  gen_cumulative_growth_rate(dt, id_cols_no_age)

  # Ratio 30d10/20d40
  gen_ratio_30d10_20d40(dt, id_cols_no_age)

  # Life expectancy at open age interval
  dt <- gen_open_interval_life_expectancy(dt, id_cols_no_age)

  # Population age a from synthetic cohort method
  gen_pop_age_a_synthetic_cohort(dt, id_cols_no_age)


  # Synthesis --------------------------------------------------------------

  # Do age trimming
  dt_fit <- dt[age_start >= age_trim_lower & age_start < age_trim_upper]

  # Get completeness
  dt_fit[, completeness := pop_age_a_synthetic_cohort / bdays_age_a]

  # Get mean
  dt_fit <- dt_fit[
    , list(completeness = mean(completeness)),
    by = id_cols_no_age
  ]

  # format and return
  output_vars <- c("completeness")
  dt_fit <- unique(dt_fit[, .SD, .SDcols = c(id_cols_no_age, output_vars)])
  results <- list(dt, dt_fit)
  return(results)

}


# Helper functions --------------------------------------------------------

# Age-specific growth rate
# (1 / t) * ln(N2(a) / N1(a)) - migrants / sqrt(pop1 * pop2)
gen_age_specific_growth_rate <- function(dt, migration) {
  dt[, t := as.numeric(difftime(date2, date1, units = "days")) / 365]
  if (migration) {
    dt[, growth_rate := (1 / t) * log(pop2 / pop1) -
           migrants / sqrt(pop1 * pop2)]
  } else {
    dt[, growth_rate := (1 / t) * log(pop2 / pop1)]
  }
}


# Cumulative growth rate
# This method is derived from Bennett and Horiuchi, Mortality Estimation from
# Registered Deaths in Less Developed Countries Demography, Vol 21, No 2 May,
# 1984 -- pp. 217-233.
# See approximation for 5da just after equation 7, page 220.
gen_cumulative_growth_rate <- function(dt, id_cols_no_age) {

  # add up growth up to age a, excluding age a
  cumsum_exclusive <- function(x) c(0, head(cumsum(x), -1))
  dt[, sum_growth := cumsum_exclusive(growth_rate), by = id_cols_no_age]

  # cumulative growth is total growth
  dt[, cumulative_growth_rate := (age_length * sum_growth) +
       ((age_length / 2) * growth_rate)]

}


# Ratio 30d10 to 20d40
# Also see Bennett and Horiuchi section noted above
gen_ratio_30d10_20d40 <- function(dt, id_cols_no_age) {

  # first use cumulative growth to get ndx
  dt[, ndx := deaths * exp(cumulative_growth_rate)]

  # then sum up over age ranges and compute ratio
  dt[, ratio_30d10_20d40 :=
       sum(ndx[age_start >= 10 & age_start < 40]) /
       sum(ndx[age_start >= 40 & age_start < 60]),
     by = id_cols_no_age]
}


# Life expectancy at open age interval
gen_open_interval_life_expectancy <- function(dt, id_cols_no_age) {

  # prep coale-demeny life tables
  cd <- copy(coale_demeny_ex)
  cd_id_cols <- c("cd_region", "sex", "index", "age_start")
  setnames(cd, c("ex", "ratio_30d10_20d40"), c("ex_1", "ratio_30d10_20d40_1"))
  cd[, ex_2 := shift(ex_1, type = "lead"), by = setdiff(cd_id_cols, "index")]
  cd[, ratio_30d10_20d40_2 := shift(ratio_30d10_20d40_1, type = "lead"),
     by = setdiff(cd_id_cols, "index")]

  # need to make a copy of r1 and r2 because the following conditional join
  # sets them equal to ratio_30d10_20d40
  cd[, `:=` (r1 = ratio_30d10_20d40_1, r2 = ratio_30d10_20d40_2)]

  # subset data to open age intervals
  dt[, open_age := max(age_start), by = id_cols_no_age]
  dt_open <- dt[age_start == max(age_start)]

  # conditional join to match level on ratio 30d10/20d40
  dt_open <- cd[
    dt_open,
    on = list(
      cd_region, sex, age_start,
      r1 >= ratio_30d10_20d40, r2 < ratio_30d10_20d40
    )
  ]
  dt_open <- dt_open[, .SD,
    .SDcols = c(id_cols_no_age, "age_start", "ex_1", "ex_2",
                "ratio_30d10_20d40_1", "ratio_30d10_20d40_2")
  ]

  # merge back onto input data.table
  # TODO: modify in place
  dt <- merge(
    dt, dt_open,
    by = c(id_cols_no_age, "age_start"),
    all.x = T
  )

  # interpolate life expectancy between two levels
  dt[, ex := ex_1 + (ratio_30d10_20d40_1 - ratio_30d10_20d40) /
       (ratio_30d10_20d40_1 - ratio_30d10_20d40_2) * (ex_2 - ex_1)]

  # remove extra columns
  dt[, c("ratio_30d10_20d40_1", "ratio_30d10_20d40_2", "ex_1", "ex_2") := NULL]

  return(dt)

}


# Population age a from synthetic cohort method
# N(a)open = deaths(a) * exp(ex * growth_rate) - [(ex * growth_rate)^2]/6
# N(a) = N(a+n) * exp(n * growth_rate) + deaths(a) * exp(0.5 * n * growth_rate)
gen_pop_age_a_synthetic_cohort <- function(dt, id_cols_no_age) {

  # start with open age interval
  dt[, open_age := max(age_start), by = id_cols_no_age]
  dt[
    age_start == open_age,
    pop_age_a_synthetic_cohort :=
      deaths * exp(ex * growth_rate) - (ex * growth_rate)^2 / 6
  ]

  # helper function to grab n-th largest value from vector
  nth_largest <- function(x, n) return(max(sort(x)[1:(length(x) - n + 1)]))

  # add age length
  dt[, age_length := shift(age_start, 1, type = "lead") - age_start,
     by = id_cols_no_age]

  # recursively compute age groups going from older to younger
  for (i in 2:length(unique(dt$age_start))) {
    dt[
      ,
      pop_age_a_synthetic_cohort_i :=
        pop_age_a_synthetic_cohort[age_start == nth_largest(age_start, i - 1)] *
        exp(age_length * growth_rate) +
        deaths * exp(0.5 * age_length * growth_rate),
      by = id_cols_no_age
    ]
    dt[
      age_start == nth_largest(age_start, i),
      pop_age_a_synthetic_cohort := pop_age_a_synthetic_cohort_i
    ]
  }
  dt[, pop_age_a_synthetic_cohort_i := NULL]

}

