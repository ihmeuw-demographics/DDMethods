
# Download Coale Demeny life tables
# Format for use in SEG methods
# Used for approximating life expectancy in open age interval

library(demogR)
library(data.table)

westF <- demogR::cdmltw(sex = "F")
westM <- demogR::cdmltw(sex = "M")
eastF <- demogR::cdmlte(sex = "F")
eastM <- demogR::cdmlte(sex = "M")
southF <- demogR::cdmlts(sex = "F")
southM <- demogR::cdmlts(sex = "M")
northF <- demogR::cdmltn(sex = "F")
northM <- demogR::cdmltn(sex = "M")

# combine all
dt <- data.table()
for(dir in c("north", "south", "east", "west")) {
  for(ss in c("M", "F")) {
    lt <- get(paste0(dir, ss))
    temp_ex <- as.data.table(lt$ex)
    temp_dx <- as.data.table(lt$ndx)
    temp_ex$parameter <- "ex"
    temp_dx$parameter <- "dx"
    temp_ex[, index := .I]
    temp_dx[, index := .I]
    temp <- rbind(temp_ex, temp_dx)
    temp <- melt(
      temp,
      id.vars = c("index", "parameter"),
      variable.name = "age_start",
      value.name = "value"
    )
    temp <- dcast(temp, index + age_start ~ parameter, value.var = "value")
    temp[, cd_region := dir]
    temp[, sex := ifelse(ss == "M", "male", "female")]
    dt <- rbind(dt, temp, fill = T)
  }
}
dt <- dt[!is.na(ex) & !is.na(dx)]

# get all-sex as simple mean
dt_all_sex <- dt[, list(
  dx = mean(dx),
  ex = mean(ex),
  sex = "all"
), by = c("index", "age_start", "cd_region")]
dt <- rbind(dt, dt_all_sex)

# get ratio 30d10 / 20d40
dt[, age_start := as.numeric(as.character(age_start))] # convert age from factor
dt[, ratio_30d10_20d40 :=
     sum(dx[data.table::between(age_start, 10, 39)]) /
     sum(dx[data.table::between(age_start, 40, 59)]),
   by = c("cd_region", "sex", "index")]

# subset columns
coale_demeny_ex <- dt[
  ,
  .SD,
  .SDcols = c("cd_region", "sex", "index", "age_start", "ex", "ratio_30d10_20d40")
]

# plot to examine
library(ggplot2)
gg <- ggplot(data = coale_demeny_ex) +
  geom_line(aes(x = ratio_30d10_20d40, y = ex, color = cd_region, lty = sex)) +
  theme_bw() +
  facet_wrap("age_start")

# save
usethis::use_data(coale_demeny_ex, overwrite = TRUE)
