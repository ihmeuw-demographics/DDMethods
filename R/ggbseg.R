
#' @title General Growth Balance - Synthetic Extinct Generations (GGBSEG) DDM
#'
#' @description Estimate completeness of death registration using two
#'   censuses with age-specific population, and average annual age-specific
#'   deaths recorded between the censuses, following the General Growth Balance
#'   & Synthetic Extinct Generations hybrid method.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data, must include columns 'pop1', 'pop2', 'deaths', 'age_start',
#'   'sex', 'date1', 'date2', 'cd_region', and all `id_cols`. Optional column
#'   'migrants' to be included if `migration = T`.
#' @param age_trim_lower_ggb \[`numeric(1)`\]\cr
#'   age-trim: lower bound of 'age_start' included for GGB (inclusive)
#' @param age_trim_upper_ggb \[`numeric(1)`\]\cr
#'   age-trim: upper bound of 'age_start' included for GGB (exclusive)
#' @param age_trim_lower_seg \[`numeric(1)`\]\cr
#'   age-trim: lower bound of 'age_start' included for SEG (inclusive).
#'   Defaults to be the same as GGB lower age trim.
#' @param age_trim_upper_seg \[`numeric(1)`\]\cr
#'   age-trim: upper bound of 'age_start' included for SEG (exclusive).
#'   defaults to be the same as GGB upper age trim.
#' @param drop_open_interval_ggb \[`logical(1)`\]\cr
#'   Whether to drop the open age interval if it falls within the age trim
#'   bounds (for GGB component).
#' @param drop_open_interval_seg \[`logical(1)`\]\cr
#'   Whether to drop the open age interval if it falls within the age trim
#'   bounds (for SEG component).
#' @inheritParams seg
#' @inheritParams ggb
#'
#' @return \[`list(2)`\]\cr
#'   * `dt` \[`data.table()`\] Input `dt` returned with additional variables
#'     computed in GGBSEG estimation, such as birthdays age a, population age
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
#' results <- ggbseg(
#'   dt,
#'   age_trim_lower_ggb = age_trim_lower,
#'   age_trim_upper_ggb = age_trim_upper,
#'   id_cols = id_cols,
#'   migration = FALSE,
#'   input_deaths_annual = FALSE,
#'   input_migrants_annual = FALSE
#' )
#'
#' @export

ggbseg <- function(dt,
                   age_trim_lower_ggb = 5,
                   age_trim_upper_ggb = 75,
                   age_trim_lower_seg = 45,
                   age_trim_upper_seg = 90,
                   drop_open_interval_ggb = T,
                   drop_open_interval_seg = T,
                   id_cols = c("age_start", "sex"),
                   migration = F,
                   input_deaths_annual = T,
                   input_migrants_annual = T,
                   method_growth_rate = "hyc",
                   method_numerator_denominator_type = "Nx") {

  # Validate and setup ------------------------------------------------------

  # check types
  checkmate::assert_data_table(dt)
  checkmate::assert_numeric(age_trim_lower_ggb, lower = 0, len = 1)
  checkmate::assert_numeric(age_trim_upper_ggb, lower = 0, len = 1)
  checkmate::assert_numeric(age_trim_lower_seg, lower = 0, len = 1)
  checkmate::assert_numeric(age_trim_upper_seg, lower = 0, len = 1)
  checkmate::assert_true(age_trim_lower_ggb < age_trim_upper_ggb)
  checkmate::assert_true(age_trim_lower_seg < age_trim_upper_seg)
  checkmate::assert_logical(drop_open_interval_ggb)
  checkmate::assert_logical(drop_open_interval_seg)
  checkmate::assert_character(id_cols)
  checkmate::assert_logical(migration, len = 1)
  checkmate::assert_choice(method_growth_rate, choices = c("hyc", "tr_iussp"))
  checkmate::assert_choice(
    method_numerator_denominator_type,
    choices = c("Nx", "nNx")
  )

  # check columns
  check_vars <- c(id_cols, "pop1", "pop2", "deaths", "date1", "date2")
  if (migration) check_vars <- c(check_vars, "migrants")
  checkmate::assert_names(names(dt), must.include = check_vars)

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


  # GGB ---------------------------------------------------------------------

  # Run GGB
  ggb_results <- ggb(
    dt = dt,
    age_trim_lower = age_trim_lower_ggb,
    age_trim_upper = age_trim_upper_ggb,
    drop_open_interval = drop_open_interval_ggb,
    id_cols = id_cols,
    migration = migration,
    input_deaths_annual = TRUE,
    input_migrants_annual = TRUE,
    method_growth_rate = method_growth_rate
  )[["completeness"]]

  dt <- merge(
    dt, ggb_results[, .SD, .SDcols = c(id_cols_no_age, "k1_over_k2")],
    by = id_cols_no_age
  )

  # Adjust census 1 using estimated coverage ratio comparing census 1 to
  #   census 2
  dt[, pop1_copy := pop1]
  dt[, pop1 := pop1 / k1_over_k2]


  # SEG ---------------------------------------------------------------------

  seg_results <- seg(
    dt = dt,
    age_trim_lower = age_trim_lower_seg,
    age_trim_upper = age_trim_upper_seg,
    drop_open_interval = drop_open_interval_seg,
    id_cols = id_cols,
    migration = migration,
    input_deaths_annual = TRUE,
    input_migrants_annual = TRUE,
    method_numerator_denominator_type = method_numerator_denominator_type
  )

  # for record keeping, change column names a bit
  setnames(seg_results[["dt"]], "pop1", "pop1_ggb_adjusted")
  setnames(seg_results[["dt"]], "pop1_copy", "pop1")

  return(seg_results)

}
