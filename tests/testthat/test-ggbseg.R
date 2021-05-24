
library(data.table)

# test using example here for males in South Africa between 2001 and 2007:
# http://demographicestimation.iussp.org/content/synthetic-extinct-generations-methods

dt <- copy(zaf_2001_2007)

id_cols <- c("location", "sex", "age_start")
age_trim_lower_ggb <- 5
age_trim_upper_ggb <- 85
age_trim_lower_seg <- 25
age_trim_upper_seg <- 65

# additional cols
dt$cd_region <- "west"


test_that("seg with migration works", {

  # run function w/ migration
  test <- ggbseg(
    dt,
    age_trim_lower_ggb = age_trim_lower_ggb,
    age_trim_upper_ggb = age_trim_upper_ggb,
    age_trim_lower_seg = age_trim_lower_seg,
    age_trim_upper_seg = age_trim_upper_seg,
    id_cols = id_cols,
    migration = T,
    input_deaths_annual = F,
    input_migrants_annual = F
  )

  expect_equivalent(test[[2]]$completeness, 0.91, tolerance = 0.01)

})

test_that("seg without migration works", {

  # run function w/o migration
  test <- ggbseg(
    dt,
    age_trim_lower_ggb = age_trim_lower_ggb,
    age_trim_upper_ggb = age_trim_upper_ggb,
    age_trim_lower_seg = age_trim_lower_seg,
    age_trim_upper_seg = age_trim_upper_seg,
    id_cols = id_cols,
    migration = F,
    input_deaths_annual = F,
    input_migrants_annual = F
  )

  expect_equivalent(test[[2]]$completeness, 0.94, tolerance = 0.02)

})


# # test Tim Riffe code
# # commented out because DDM package has a lot of dependencies, and this test
# # does not need to be run regularly with package compilation

# dt_tr <- copy(dt)
# dt_tr[, cod := "South_Africa_male"]
# setnames(dt_tr, "age_start", "age")
# # no migration (gives us 0.937)
# test <- DDM::ggbseg(
#   dt_tr,
#   exact.ages.ggb = seq(5, 80, 5),
#   exact.ages.seg = seq(25, 60, 5),
#   deaths.summed = T
# )
# # with migration (gives us 0.912)
# setnames(dt_tr, "migrants", "mig")
# test <- DDM::ggbseg(
#   dt_tr,
#   exact.ages.ggb = seq(5, 80, 5),
#   exact.ages.seg = seq(25, 60, 5),
#   deaths.summed = T
# )
