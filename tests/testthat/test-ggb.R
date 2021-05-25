
library(data.table)
library(ggplot2)
library(ggrepel)

# test using example here for males in South Africa between 2001 and 2007:
# http://demographicestimation.iussp.org/content/generalized-growth-balance-method

dt <- copy(zaf_2001_2007)

id_cols <- c("location", "sex", "age_start")
age_trim_lower = 5
age_trim_upper = 85


test_that("ggb with migration works", {

  # run function w/ migration
  test <- ggb(
    dt,
    age_trim_lower = age_trim_lower,
    age_trim_upper = age_trim_upper,
    id_cols = id_cols,
    migration = T,
    input_deaths_annual = F,
    input_migrants_annual = F
  )

  expect_equivalent(test[[2]]$completeness, 0.9054, tolerance = 0.001)

})

test_that("ggb without migration works", {

  # run function w/o migration
  test <- ggb(
    dt,
    age_trim_lower = age_trim_lower,
    age_trim_upper = age_trim_upper,
    id_cols = id_cols,
    migration = F,
    input_deaths_annual = F,
    input_migrants_annual = F
  )

  expect_equivalent(test[[2]]$completeness, 0.9333, tolerance = 0.001)

})

test_that("ggb plotting works", {

  # run function w/o migration
  test <- ggb(
    dt,
    age_trim_lower = age_trim_lower,
    age_trim_upper = age_trim_upper,
    id_cols = id_cols,
    migration = T,
    input_deaths_annual = F,
    input_migrants_annual = F
  )

  id_cols_subset <- list(
    "location" = "South Africa",
    "sex" = "male",
    "age_start" = c(5:80)
  )

  gg <- expect_silent(plot_ggb(test, id_cols_subset))
  expect_equal("ggplot" %in% class(gg), TRUE)

})


# # test Tim Riffe code
# # commented out because DDM package has a lot of dependencies, and this test
# # does not need to be run regularly with package compilation

# dt_tr <- copy(dt)
# dt_tr[, cod := "South_Africa_male"]
# setnames(dt_tr, "age_start", "age")
# # no migration (gives us 0.933)
# test <- DDM::ggb(
#   dt_tr,
#   exact.ages = seq(5, 80, 5),
#   deaths.summed = T,
#   lm.method = "oldschool"
# )
# # with migration (gives us 0.906)
# setnames(dt_tr, "migrants", "mig")
# test <- DDM::ggb(
#   dt_tr,
#   exact.ages = seq(5, 80, 5),
#   deaths.summed = T,
#   lm.method = "oldschool"
# )
#
# # run our function w/o migration and with TR growth rate method
# test <- ggb(
#   dt,
#   age_trim_lower = age_trim_lower,
#   age_trim_upper = age_trim_upper,
#   id_cols = id_cols,
#   migration = F,
#   input_deaths_annual = F,
#   input_migrants_annual = F,
#   method_growth_rate = "b"
# )
