
library(data.table)
library(ggplot2)
library(ggrepel)

# test using example here for males in South Africa between 2001 and 2007:
# http://demographicestimation.iussp.org/content/generalized-growth-balance-method

dt <- copy(zaf_2001_2007)

setnames(dt, "age", "age_start")
id_cols <- c("location", "sex", "age_start")
age_trim_lower = 5
age_trim_upper = 85

# convert from total to average annual deaths & migrants
dt[, deaths := deaths / 5.353425]
dt[, migrants := migrants / 5.353425]


test_that("ggb with migration works", {

  # run function w/ migration
  test <- ggb(
    dt,
    age_trim_lower = age_trim_lower,
    age_trim_upper = age_trim_upper,
    id_cols = id_cols,
    migration = T
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
    migration = F
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
    migration = T
  )

  id_cols_subset <- list(
    "location" = "South Africa",
    "sex" = "male",
    "age_start" = c(5:80)
  )

  gg <- expect_silent(plot_ggb(test, id_cols_subset))
  expect_equal("ggplot" %in% class(gg), TRUE)

})



