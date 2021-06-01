# data.table is generally careful to minimize the scope for namespace
# conflicts (i.e., functions with the same name as in other packages);
# a more conservative approach using @importFrom should be careful to
# import any needed data.table special symbols as well, e.g., if you
# run DT[ , .N, by='grp'] in your package, you'll need to add
# @importFrom data.table .N to prevent the NOTE from R CMD check.
# See ?data.table::`special-symbols` for the list of such symbols
# data.table defines; see the 'Importing data.table' vignette for more
# advice (vignette('datatable-importing', 'data.table')).
#
#' @import data.table
NULL

# this is needed for non-standard evaluation in data.table (and some other
# packages). Multiple links suggest using `utils::globalVariables` to remove
# notes when checking the package.
# https://www.r-bloggers.com/no-visible-binding-for-global-variable/
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c("age_length", "age_start", "bdays_age_a",
                         "completeness", "date1", "date2", "death_rate",
                         "deaths", "entry_minus_growth", "entry_rate",
                         "growth_rate", "intercept", "k1", "k1_over_k2",
                         "k2", "mig_rate", "migrants", "pop1",
                         "pop1_aminus1", "pop2", "pop_age_aplus", "slope",
                         "sd", "cd_region", "coale_demeny_ex",
                         "cumulative_growth_rate", "ex", "ex_1", "ex_2",
                         "head", "id_cols", "ndx", "open_age",
                         "pop_age_a_synthetic_cohort",
                         "pop_age_a_synthetic_cohort_i", "r1", "r2",
                         "ratio_30d10_20d40", "ratio_30d10_20d40_1",
                         "ratio_30d10_20d40_2", "sex", "sum_growth",
                         "pop1_copy", "pop_from_deaths", "pop_from_censuses",
                         "ith_largest", "n_ages"))

