
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
#' @param id_cols \[`character()`\]\cr
#'   Column names that uniquely identify rows of `dt`.
#'   Must include 'age_start' and 'sex'. When 'age_start' is removed, these
#'   columns define the groups to calculate completeness for.
#' @param method_numerator_denominator_type \[`character()`\]\cr
#'   Completeness point estimates are population estimated from deaths
#'   divided by population estimated from censuses. This setting dictates
#'   the ages the population belongs to. Must be one of:
#'   * "Nx" : numerator and denominator are both estimated population
#'        aged x. This is the default and matches the GBD methodology.
#'   * "nNx" : numerator and denominator are both estimated population
#'        aged x to x+n. This matches Tim Riffe DDM package and IUSSP methods.
#' @inheritParams ggb
#'
#' @return \[`list(2)`\]\cr
#'   * `dt` \[`data.table()`\] Input `dt` returned with additional variables
#'     computed in SEG estimation, such as birthdays age a, population age
#'     a plus, and synthetic cohort population age a plus.
#'   * `completeness` \[`data.table()`\] Estimated completeness by unique
#'     combination of `id_cols`.
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
#' @inherit ggb references
#'
#' @examples
#' library(data.table)
#' dt <- copy(zaf_2001_2007)
#' dt[, cd_region := "west"]
#' id_cols <- c("location", "sex", "age_start")
#' age_trim_lower = 25
#' age_trim_upper = 65
#' results <- seg(
#'   dt,
#'   age_trim_lower = age_trim_lower,
#'   age_trim_upper = age_trim_upper,
#'   id_cols = id_cols,
#'   migration = FALSE,
#'   input_deaths_annual = FALSE,
#'   input_migrants_annual = FALSE
#' )
#'
#' @export

seg <- function(dt,
                age_trim_lower = 45,
                age_trim_upper = 90,
                drop_open_interval = T,
                id_cols = c("age_start", "sex"),
                migration = F,
                input_deaths_annual = T,
                input_migrants_annual = T,
                method_numerator_denominator_type = "Nx") {

  # Validate and setup ------------------------------------------------------

  # check types
  checkmate::assert_data_table(dt)
  checkmate::assert_numeric(age_trim_lower, lower = 0, len = 1)
  checkmate::assert_numeric(age_trim_upper, lower = 0, len = 1)
  checkmate::assert_true(age_trim_lower < age_trim_upper)
  checkmate::assert_logical(drop_open_interval)
  checkmate::assert_character(id_cols)
  checkmate::assert_logical(migration, len = 1)
  checkmate::assert_choice(
    method_numerator_denominator_type,
    choices = c("Nx", "nNx")
  )

  # check columns
  check_vars <- c(id_cols, "pop1", "pop2", "deaths", "date1", "date2", "cd_region")
  if (migration) check_vars <- c(check_vars, "migrants")
  checkmate::assert_names(names(dt), must.include = check_vars)
  checkmate::assert_character(dt$cd_region, pattern = "north|south|west|east")
  checkmate::assert_character(dt$sex, pattern = "male|female|all")

  # convert lingering integers to doubles
  dt[, c("pop1", "pop2", "deaths") := lapply(.SD, as.double),
     .SDcols = c("pop1", "pop2", "deaths")]

  # check ID cols
  checkmate::assert_names(id_cols, must.include = c("age_start", "sex"))
  demUtils::assert_is_unique_dt(dt, id_cols)

  # light prep
  dt <- copy(dt)
  id_cols_no_age <- setdiff(id_cols, "age_start")
  setorderv(dt, c(id_cols_no_age, "age_start"))

  # convert to annualized deaths and net migrants
  dt[, t := as.numeric(difftime(date2, date1, units = "days")) / 365]
  if (!input_deaths_annual) dt[, deaths := deaths / t]
  if (migration & !input_migrants_annual) dt[, migrants := migrants / t]

  # add age length
  dt[, age_length := shift(age_start, 1, type = "lead") - age_start,
     by = id_cols_no_age]
  dt[, age_length2 := age_start - shift(age_start, 1, type = "lag"),
     by = id_cols_no_age] # fill in terminal age group
  dt[is.na(age_length), age_length := age_length2]
  dt[, c("age_length2") := NULL]


  # Compute components ------------------------------------------------------

  # Denominator: population from censuses
  gen_pop_denominator(dt, id_cols_no_age, method_numerator_denominator_type)

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
  gen_open_interval_life_expectancy(dt)

  # Numerator: population from deaths via synthetic cohort method
  gen_pop_numerator(dt, id_cols_no_age, method_numerator_denominator_type)


  # Synthesis --------------------------------------------------------------

  # Do age trimming
  # use specified age trim, but also remove terminal age group if below the
  #   upper trim
  dt_fit <- dt[age_start >= age_trim_lower & age_start < age_trim_upper]
  if (drop_open_interval) dt_fit <- dt_fit[age_start < open_age]

  # Get completeness
  dt_fit[, completeness := pop_from_deaths / pop_from_censuses]

  # Get mean
  dt_fit <- dt_fit[
    , list(completeness = mean(completeness)),
    by = id_cols_no_age
  ]

  # format and return
  output_vars <- c("completeness")
  dt_fit <- unique(dt_fit[, .SD, .SDcols = c(id_cols_no_age, output_vars)])
  results <- list("dt" = dt, "completeness" = dt_fit)
  return(results)

}


# Helper functions --------------------------------------------------------

# Denominator: population from censuses
# Method = "Nx" : pop ~ birthdays age a
# Method = "nNx" : pop ~ geometric mean of pop from census 1 and census 2
gen_pop_denominator <- function(dt,
                                id_cols_no_age,
                                method_numerator_denominator_type) {

  if (method_numerator_denominator_type == "Nx") {
    gen_bdays_age_a(dt, id_cols_no_age)
    setnames(dt, "bdays_age_a", "pop_from_censuses")

  } else if (method_numerator_denominator_type == "nNx") {
    dt[, pop_from_censuses := sqrt(pop1 * pop2)]

  } else {
    stop("'method_numerator_denominator_type' must be 'Nx' or 'nNx'.")
  }
}

# Age-specific growth rate
# (1 / t) * ln(N2(a) / N1(a)) - migrants / sqrt(N1(a) * N2(a))
gen_age_specific_growth_rate <- function(dt, migration) {
  if (migration) {
    dt[, growth_rate := (1 / t) * log(pop2 / pop1) -
           migrants / sqrt(pop1 * pop2)]
  } else {
    dt[, growth_rate := (1 / t) * log(pop2 / pop1)]
  }
}


# Cumulative growth rate
# This method is derived from Bennett and Horiuchi (1984). See approximation
#   for 5da just after equation 7, page 220.
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
gen_open_interval_life_expectancy <- function(dt) {

  # create function which will linearly interpolate CD life tables to
  #  get life expectancy from ratio 30d10 to 20d40
  r_to_ex <- function(r, ss, aa, cdr) {
    f <- stats::approxfun(
      x = coale_demeny_ex[sex == ss & age_start == aa & cd_region == cdr]$ratio_30d10_20d40,
      y = coale_demeny_ex[sex == ss & age_start == aa & cd_region == cdr]$ex,
      method = "linear",
      rule = 2 # constant extrapolation from the ends
    )
    ex <- f(r)
    return(ex)
  }

  # apply function over all ID columns of CD life tables (region, sex, age)
  for (cdr in c("west", "east", "north", "south")) {
    for (ss in c("male", "female", "all")) {
      for (aa in unique(dt$age_start)) {
        dt[sex == ss & age_start == aa & cd_region == cdr,
           ex := r_to_ex(ratio_30d10_20d40, ss, aa, cdr)]
      }
    }
  }

  # check for missingness
  checkmate::assert_numeric(dt$ex, any.missing = F)

  return(dt)

}


# Numerator: Population from deaths via synthetic cohort method
# N(a)open = deaths(a) * exp(ex * growth_rate) - [(ex * growth_rate)^2]/6
# N(a) = N(a+n) * exp(n * growth_rate) + deaths(a) * exp(0.5 * n * growth_rate)
# Method = "Nx": Nx = N(a) as described
# Method = "nNx": nNx = (N(a) + N(a+n)) * (n / 2)
gen_pop_numerator <- function(dt,
                              id_cols_no_age,
                              method_numerator_denominator_type) {

  # start with open age interval
  dt[, open_age := max(age_start), by = id_cols_no_age]
  dt[
    age_start == open_age,
    pop_age_a_synthetic_cohort :=
      deaths * (exp(ex * growth_rate) - (ex * growth_rate)^2 / 6)
  ]

  # helper function to grab n-th largest value from vector
  nth_largest <- function(x, n) return(max(sort(x)[1:(length(x) - n + 1)]))

  # recursively compute age groups going from older to younger
  dt[, n_ages := uniqueN(age_start), by = id_cols_no_age]
  for (i in 2:length(unique(dt$age_start))) {
    dt[
      n_ages >= i,
      pop_age_a_synthetic_cohort_i :=
        pop_age_a_synthetic_cohort[age_start == nth_largest(age_start, i - 1)] *
        exp(age_length * growth_rate) +
        deaths * exp(0.5 * age_length * growth_rate),
      by = id_cols_no_age
    ]
    dt[
      n_ages >= i,
      ith_largest := nth_largest(age_start, i),
      by = id_cols_no_age
    ]
    dt[
      n_ages >= i & age_start == ith_largest,
      pop_age_a_synthetic_cohort := pop_age_a_synthetic_cohort_i
    ]
  }
  dt[, c("pop_age_a_synthetic_cohort_i", "ith_largest") := NULL]

  # set 'pop_from_deaths' based on numerator & denominator type chosen
  if (method_numerator_denominator_type == "Nx") {
    dt[, pop_from_deaths := pop_age_a_synthetic_cohort]
  } else if (method_numerator_denominator_type == "nNx") {
    dt[, pop_from_deaths :=
         (pop_age_a_synthetic_cohort +
            shift(pop_age_a_synthetic_cohort, type = "lead")) *
         (age_length / 2),
       by = id_cols_no_age]
  } else {
    stop("'method_numerator_denominator_type' must be 'Nx' or 'nNx'.")
  }

}

