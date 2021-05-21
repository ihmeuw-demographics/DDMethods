
library(data.table)
library(ggplot2)
library(ggrepel)

# test using example here for males in South Africa between 2001 and 2007:
# http://demographicestimation.iussp.org/content/synthetic-extinct-generations-methods

dt <- copy(zaf_2001_2007)

setnames(dt, "age", "age_start")
id_cols <- c("location", "sex", "age_start")
age_trim_lower <- 25
age_trim_upper <- 65

# convert from total to average annual deaths & migrants
dt[, deaths := deaths / 5.353425]
dt[, migrants := migrants / 5.353425]

# additional cols
dt$cd_region <- "west"


test_that("seg with migration works", {

  # run function w/ migration
  test <- seg(
    dt,
    age_trim_lower = age_trim_lower,
    age_trim_upper = age_trim_upper,
    id_cols = id_cols,
    migration = T
  )

  expect_equivalent(test[[2]]$completeness, 1.03, tolerance = 0.01)

})

test_that("seg without migration works", {

  # run function w/o migration
  test <- seg(
    dt,
    age_trim_lower = age_trim_lower,
    age_trim_upper = age_trim_upper,
    id_cols = id_cols,
    migration = F
  )

  expect_equivalent(test[[2]]$completeness, 1.09, tolerance = 0.02)

})


# # test Tim Riffe code
# # commented out because DDM package has a lot of dependencies, and this test
# # does not need to be run regularly with package compilation

# dt_tr <- copy(dt)
# dt_tr[, cod := "South_Africa_male"]
# setnames(dt_tr, "age_start", "age")
# # no migration (gives us 1.079)
# test <- DDM::seg(dt_tr, exact.ages = seq(25, 60, 5))
# # with migration (gives us 1.026)
# setnames(dt_tr, "migrants", "mig")
# test <- DDM::seg(dt_tr, exact.ages = seq(25, 60, 5))
