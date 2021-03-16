library(data.table)

minors <- fread("minor-league-stats.csv")
first_yrs <- fread("pitchers-1-yrs.csv")
second_yrs <- fread("pitchers-2-yrs.csv")
third_yrs <- fread("pitchers-3-yrs.csv")
fourth_yrs <- fread("pitchers-4-yrs.csv")
fifth_yrs <- fread("pitchers-5-yrs.csv")

minors <- minors[gs >= 10 & age <= 26]
#### Predicting major league debut based on minor league stats ####
# Ensure a sample size and remove rehab stints and minor league deals after 
# major league debut by setting an age limit
first_yrs <- first_yrs[year_in_mlb == 1 & age <= 26][season >= 2006][
  name %in% minors$name]
minors_first_yr <- minors[name %in% first_yrs$name]

vars <- c("age","w", "l", "gs",
          "ip", "h", "r", "er",
          "hr", "bb", "so", "fip")
minor_reg <- reg_df(minors_first_yr, vars, 1, "ip")

vars <- append(vars, c("babip", "war"))
first_yrs_reg <- reg_df(first_yrs, vars, 1, "ip")
names(first_yrs_reg) <- gsub(x = names(first_yrs_reg), pattern = "_year1",
                             replacement = "_year2")
first_yrs_reg <- cbind(minor_reg, first_yrs_reg)

#### One year in majors + minor league experience ####
second_yrs <- second_yrs[, .SD[c(1,2)], by = name][season >= 2006][
  name %in% minors$name]

minors_second_yr <- minors[name %in% second_yrs$name]



