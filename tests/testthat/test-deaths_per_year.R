
# get manual comparison
annual_deaths_manual_1 <- mean(
  c(100:105, rep(105, 5))
)
annual_deaths_manual_2 <- mean(
  c(200:205, rep(205, 5))
)

testthat::test_that("`deaths_per_year` works", {
  annual_deaths <- testthat::expect_silent(
    deaths_per_year(
      years_deaths = c(2000, 2005),
      input_deaths = c(100, 105),
      years_census = c(2000, 2010),
      step_size = 1
    )
  )
  expect_equal(annual_deaths, annual_deaths_manual_1)
})

testthat::test_that("`deaths_per_year` works inside a data.table", {
  # setup input
  dt <- data.table(
    deaths = c(100, 105, 200, 205),
    year = c(2000, 2005, 2000, 2005),
    year1 = 2000,
    year2 = 2010,
    pop1 = c(3000, 3000, 3600, 3600),
    pop2 = c(3100, 3100, 3700, 3700),
    sex = c("male", "male", "female", "female")
  )
  # collapse to average deaths per year
  dt <- testthat::expect_silent(dt[
    ,
    list(
      deaths = deaths_per_year(
        years_deaths = year,
        input_deaths = deaths,
        years_census = c(unique(year1), unique(year2)),
        step_size = 1
      ),
      pop1 = unique(pop1),
      pop2 = unique(pop2),
      deaths_years = paste(sort(unique(year)), collapse = " "),
      pop_years = paste(unique(year1), unique(year2), collapse = " ")
    ),
    by = "sex"
  ])
  expected_output <- data.table(
    sex = c("male", "female"),
    deaths = c(annual_deaths_manual_1, annual_deaths_manual_2),
    pop1 = c(3000, 3600),
    pop2 = c(3100, 3700),
    deaths_years = "2000 2005",
    pop_years = "2000 2010"
  )
  testthat::expect_equal(dt, expected_output)

})
