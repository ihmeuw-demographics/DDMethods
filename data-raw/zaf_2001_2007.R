


# Load test data file from internet
file <- "http://demographicestimation.iussp.org/sites/demographicestimation.iussp.org/files/AM_GGB_South%20Africa_males_4_0.xlsx"
tmp <- tempfile(fileext = ".xlsx")
download.file(url = file, destfile = tmp, mode = "wb")
dt <- readxl::read_excel(tmp, sheet = 3, range = "B6:F25")

# format
names(dt) <- c("age", "pop1", "pop2", "deaths", "mig")
dt <- setDT(dt)
dt <- dt[!1] # remove unneeded 1st row
dt[18, age := 85]
dt[, `:=` (
  sex = "male",
  location = "South Africa",
  date1 = lubridate::ymd("2001-10-10"),
  date2 = lubridate::ymd("2007-02-15")
)]
dt <- dt[, .SD, .SDcols = c("location", "pop1", "pop2", "deaths", "age",
                            "sex", "date1", "date2")]
zaf_2001_2007 <- copy(dt)

# save
usethis::use_data(zaf_2001_2007, overwrite = TRUE)
