
#' @title General Growth Balance (GGB) DDM
#'
#' @description Estimate completeness of death registration using two
#'   censuses with age-specific population, and average annual age-specific
#'   deaths recorded between the censuses, following the General Growth Balance
#'   method.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data, must include columns 'pop1', 'pop2', 'deaths', 'age_start',
#'   'date1', 'date2', and all `id_cols`. Optional column 'migrants' to be
#'   included if `migration = T`.
#' @param age_trim_lower \[`numeric(1)`\]\cr
#'   age-trim: lower bound of 'age_start' included (inclusive)
#' @param age_trim_upper \[`numeric(1)`\]\cr
#'   age-trim: upper bound of 'age_start' included (exclusive)
#' @param id_cols \[`character()`\]\cr
#'   Column names that uniquely identify rows of `dt`.
#'   One column must be 'age_start'. When 'age_start' is removed, these
#'   columns define the groups to calculate completeness for.
#' @param migration \[`logical(1)`\]\cr
#'   Whether a 'migrants' column should be used. If migration
#'   is available and of reasonable quality, it improves the DDM estimation.
#'   However, these data are not often available.
#' @param input_deaths_annual \[`logical(1)`\]\cr
#'   Whether input deaths are formatted as annual average recorded per year.
#'   Alternative is recorded deaths for the entire period between census 1
#'   and census 2. Default TRUE and assumes deaths are annual. If FALSE,
#'   deaths will be divided by the decimal number of years between 'date1' and
#'   'date2'.
#' @param input_migrants_annual \[`logical(1)`\]\cr
#'   Whether input migrants are formatted as annual average recorded per year.
#'   Alternative is recorded net migrants for the entire period between census 1
#'   and census 2. Default TRUE and assumes net migrants are annual. If FALSE,
#'   migrants will be divided by the decimal number of years between 'date1' and
#'   'date2'.
#' @param method_growth_rate \[`character(1)`\]\cr
#'   Method to use for computation of growth rate. One of:
#'   * "hyc": Equation 8c from Hill, You, Choi (2009).
#'       \eqn{r(a+) = (1 / t) * ln(N2(a+) / N1(a+))}.
#'   * "tr_iussp": Equation from IUSSP website and Tim Riffe DDM package.
#'       \eqn{r(a+) = (1 / t) * (N2(a+) - N1(a+)) / (exp((log(N1(a+)) +
#'        log(N2(a+))) / 2))}.
#'
#' @return \[`list(2)`\]\cr
#'   * \[`data.table()`\] Input `dt` returned with additional variables
#'     computed in GGB estimation, such as birthdays age a, population age
#'     a plus, entry rate, growth rate, and partial death rate.
#'   * \[`data.table()`\] Slope, intercept, and estimated completeness by
#'     unique combination of `id_cols`.
#'
#' @details
#' Columns of `dt`:
#'   * 'pop1': age-specific population recorded at census 1
#'   * 'pop2': age-specific population recorded at census 2
#'   * 'deaths': average annual deaths recorded between census 1 and census 2,
#'     or, if 'input_deaths_annual' is FALSE, total deaths recorded between
#'     census 1 and census 2
#'   * 'age_start': lower bound of age interval
#'   * 'date1': date of first census, format yyyy-mm-dd
#'   * 'date2': date of second census, format yyyy-mm-dd
#'   * 'migrants' (optional): average annual net migrants between census 1 and
#'       census 2, or, if 'input_migrants_annual' is FALSE, total net migrants
#'       recorded between census 1 and census 2
#'
#' @references
#' Methods are based on the following sources:
#' * Bennett NG, Horiuchi S. Mortality estimation from registered deaths in
#'   less developed countries. Demography. 1984 May;21(2):217-33.
#' * Dorrington RE. 2013. "The Generalized Growth Balance Method".
#'   In Moultrie TA, RE Dorrington, AG Hill, K Hill, IM Timæus and B Zaba (eds).
#'   Tools for Demographic Estimation. Paris: International Union for the
#'   Scientific Study of Population.
#'   http://demographicestimation.iussp.org/content/generalized-growth-balance-method.
#'   Accessed May 27 2021.
#' * Dorrington RE. 2013. "Synthetic extinct generations methods".
#'   In Moultrie TA, RE Dorrington, AG Hill, K Hill, IM Timæus and B Zaba (eds).
#'   Tools for Demographic Estimation. Paris: International Union for the
#'   Scientific Study of Population.
#'   http://demographicestimation.iussp.org/content/synthetic-extinct-generations-methods.
#'   Accessed May 27 2021.
#' * Hill K, You D, Choi Y. Death distribution methods for estimating adult
#'   mortality: sensitivity analysis with simulated data errors. Demographic
#'   Research. 2009 Jul 1;21:235-54.
#' * Murray CJ, Rajaratnam JK, Marcus J, Laakso T, Lopez AD. What can we
#'   conclude from death registration? Improved methods for evaluating
#'   completeness. PLoS Med. 2010 Apr 13;7(4):e1000262.
#' * Tim Riffe, Everton Lima, and Bernardo Queiroz (2017). DDM: Death
#'   Registration Coverage Estimation. R package version 1.0.0.
#'   https://CRAN.R-project.org/package=DDM
#'
#' @examples
#' library(data.table)
#' dt <- copy(zaf_2001_2007)
#' id_cols <- c("location", "sex", "age_start")
#' age_trim_lower = 5
#' age_trim_upper = 85
#' results <- ggb(
#'   dt,
#'   age_trim_lower = age_trim_lower,
#'   age_trim_upper = age_trim_upper,
#'   id_cols = id_cols,
#'   migration = TRUE,
#'   input_deaths_annual = FALSE,
#'   input_migrants_annual = FALSE
#' )
#'
#' @export

ggb <- function(dt,
                age_trim_lower = 5,
                age_trim_upper = 75,
                id_cols = "age_start",
                migration = F,
                input_deaths_annual = T,
                input_migrants_annual = T,
                method_growth_rate = "hyc") {

  # Validate and setup ------------------------------------------------------

  # check types
  checkmate::assert_data_table(dt)
  checkmate::assert_numeric(age_trim_lower, lower = 0, len = 1)
  checkmate::assert_numeric(age_trim_upper, lower = 0, len = 1)
  checkmate::assert_true(age_trim_lower < age_trim_upper)
  checkmate::assert_character(id_cols)
  checkmate::assert_logical(migration, len = 1)
  checkmate::assert_logical(input_deaths_annual, len = 1)
  checkmate::assert_logical(input_migrants_annual, len = 1)
  checkmate::assert_choice(method_growth_rate, choices = c("hyc", "tr_iussp"))

  # check columns
  check_vars <- c(id_cols, "pop1", "pop2", "deaths", "date1", "date2")
  if (migration) check_vars <- c(check_vars, "migrants")
  checkmate::assert_names(names(dt), must.include = check_vars)

  # convert lingering integers to doubles
  dt[, c("pop1", "pop2", "deaths") := lapply(.SD, as.double),
     .SDcols = c("pop1", "pop2", "deaths")]

  # check ID cols
  checkmate::assert_names(id_cols, must.include = "age_start")
  demUtils::assert_is_unique_dt(dt, id_cols)

  # light prep
  dt <- copy(dt)
  id_cols_no_age <- setdiff(id_cols, "age_start")
  setorderv(dt, c(id_cols_no_age, "age_start"))

  # convert to annualized deaths and net migrants
  dt[, t := as.numeric(difftime(date2, date1, units = "days")) / 365]
  if (!input_deaths_annual) dt[, deaths := deaths / t]
  if (migration & !input_migrants_annual) dt[, migrants := migrants / t]


  # Compute components ------------------------------------------------------

  # Entry rate
  gen_bdays_age_a(dt, id_cols_no_age)
  gen_pop_age_aplus(dt, id_cols_no_age)
  dt[, entry_rate := bdays_age_a / pop_age_aplus]

  # Growth rate
  gen_growth_rate(dt, id_cols_no_age, method = method_growth_rate)

  # Death rate: based on observed deaths not complete deaths
  gen_death_rate(dt, id_cols_no_age)

  # Migration rate (optional)
  if (migration) gen_mig_rate(dt, id_cols_no_age)


  # Regression synthesis ----------------------------------------------------

  # Prep lefthand side of balancing equation
  dt[, entry_minus_growth := entry_rate - growth_rate]

  # Include migration (optional)
  if (migration) dt[, entry_minus_growth := entry_minus_growth + mig_rate]

  # Do age trimming
  # use specified age trim, but also remove terminal age group if below the
  #   upper trim
  dt_fit <- dt[age_start >= age_trim_lower & age_start < age_trim_upper &
                 !is.na(entry_minus_growth)]

  # Orthogonal regression: entry - growth = k + 1/c * death
  # equivalent to Tim Riffe DDM package "oldschool" method
  dt_fit[, slope := stats::sd(entry_minus_growth) / stats::sd(death_rate),
         by = id_cols_no_age]
  dt_fit[, intercept := mean(entry_minus_growth) - mean(death_rate) * slope,
         by = id_cols_no_age]

  # relative completeness of census 1 compared to census 2
  dt_fit[, k1_over_k2 := exp(t * intercept)]

  # assign more complete to 1
  dt_fit[, k1 := ifelse(k1_over_k2 > 1, 1, k1_over_k2)]
  dt_fit[, k2 := k1 / k1_over_k2]

  # compute completeness of VR relative to more complete census
  dt_fit[, completeness := sqrt(k1 * k2) / slope]

  # format and return
  output_vars <- c("slope", "intercept", "completeness", "k1_over_k2")
  dt_fit <- unique(dt_fit[, .SD, .SDcols = c(id_cols_no_age, output_vars)])
  results <- list(dt, dt_fit)
  return(results)

}


# Helper functions --------------------------------------------------------

# Sum ages a+ ... kind of inverse of a cumulative sum
sum_aplus <- function(x) return(rev(cumsum(rev(x))))

# Birthdays age a
# Average birthdays age a per year, geometric mean of population aged a in interval
# (1/n) * sqrt(nN1a-n * nN2a)
gen_bdays_age_a <- function(dt, id_cols_no_age) {
  dt[, pop1_aminus1 := shift(pop1, 1, type = "lag"), by = id_cols_no_age]
  dt[, age_length := shift(age_start, 1, type = "lead") - age_start,
     by = id_cols_no_age]
  dt[, bdays_age_a := (1 / age_length) * sqrt(pop1_aminus1 * pop2)]
  dt[, pop1_aminus1 := NULL]
}

# Population age a+
# Geometric mean of population age a+ from census 1 and census 2
# sqrt(N1(a+) * N2(a+))
gen_pop_age_aplus <- function(dt, id_cols_no_age) {
  dt[, pop_age_aplus := sqrt(sum_aplus(pop1) * sum_aplus(pop2)),
     by = id_cols_no_age]
}

# Growth rate
# Method A: (1 / t) * ln(N2(a+) / N1(a+))
# Method B: (1 / t) * (N2(a+) - N1(a+)) / (exp((ln(N1(a+)) + ln(N2(a+))) / 2))
gen_growth_rate <- function(dt, id_cols_no_age, method = "hyc") {
  if (method == "hyc") {
    # equation 8c in Hill, You, Choi (2009)
    dt[, growth_rate := (1 / t) * log(sum_aplus(pop2) / sum_aplus(pop1)),
       by = id_cols_no_age]
  } else if (method == "tr_iussp") {
    # equation from IUSSP & Tim Riffe DDM package
    dt[, growth_rate := (1 / t) * (sum_aplus(pop2) - sum_aplus(pop1)) /
         (exp((log(sum_aplus(pop1)) + log(sum_aplus(pop2))) / 2)),
       by = id_cols_no_age]
  } else {
    stop("Method must be 'hyc' or 'tr_iussp'.")
  }
}

# Death rate
# (1 / completeness) * D(a+)/N(a+) -- just include D/N at this point
# D(a+) is average annual recorded deaths age a+ between census 1 and census 2
gen_death_rate <- function(dt, id_cols_no_age) {
  dt[, death_rate := sum_aplus(deaths) / pop_age_aplus, by = id_cols_no_age]
}

# Migration rate
# average annual migrants a+ divided by pop a+
gen_mig_rate <- function(dt, id_cols_no_age) {
  dt[, mig_rate := sum_aplus(migrants) / pop_age_aplus, by = id_cols_no_age]
}


# Plotting function -------------------------------------------------------

#' @title Plot GGB regression
#'
#' @description Return ggplot2 object with plot for GGB regression using
#'   outputs from `ggb` function.
#'
#' @param results \[`list(2)`\]\cr
#'   Output from `ggb` function
#' @param id_cols_subset \[`list()`\]\cr
#'   Named list where names are `id_cols` and each entry has the value(s) of
#'   the ID columns to be plotted.
#'   Ex: id_cols_subset = list(location = "South Africa", sex = "male",
#'   age_start = seq(5, 80, 5)).
#'
#' @return ggplot2 plot object to display the GGB regression inputs and
#'   linear fit.
#'
#' @export

plot_ggb <- function(results, id_cols_subset) {

  dt <- results[[1]]
  fit <- results[[2]]

  # subset based on id_cols
  for (col in names(id_cols_subset)) {
    dt <- dt[get(col) %in% id_cols_subset[[col]]]
    if (col != "age_start") fit <- fit[get(col) %in% id_cols_subset[[col]]]
  }

  y_title <- ifelse(
    "mig_rate" %in% names(dt),
    "Entry rate minus growth rate plus migration rate (age a+)",
    "Entry rate minus growth rate (age a+)"
  )
  gg <- ggplot2::ggplot(data = dt,
               ggplot2::aes(x = death_rate, y = entry_minus_growth)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = fit$slope, intercept = fit$intercept, color = "blue") +
    ggrepel::geom_text_repel(ggplot2::aes(label = age_start)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Partial death rate (age a+)", y = y_title)

  return(gg)
}

