library(data.table)
library(janitor)

master_raw <- read.csv("master-df.csv")

master <- clean_names(master_raw)
setDT(master)
setnames(master, c("i_season", "playerid"), c("season", "player_id"))   

# adds a count for how many years player has been in MLB
setorder(master, season)
master[, year_in_mlb := seq_along(1:.N), by = name]

# filter out relief pitchers and pitchers with fewer than seven games started
master <- master[gs >= 7]

# split the players into groups: one group for 5 consecutive years, 
# one group for 10, one for 15, and one for 20

# function to subset dt based on how many consecutive years in mlb
# is wanted
con_yrs <- function(df, num_years, multiple) {
  min <- num_years - multiple
  df[year_in_mlb <= num_years][,
     sum_yrs := sum(year_in_mlb),
      by = player_id][sum_yrs == sum(seq(num_years))][
        year_in_mlb > min
      ]
}

# get our list of players to regress later on
years <- c(5, 10, 15, 20)
pitchers_1 <- lapply(years, con_yrs, df = master, multiple = 5)

pitchers_1 <- rbindlist(pitchers_1)
setorder(pitchers_1, name) # order by name recommended to simplify future steps

# write.csv(pitchers_1, "pitchers-5-yrs.csv")

years <- c(4, 8, 12, 16)
pitchers_2 <- lapply(years, con_yrs, df = master, multiple = 4)

pitchers_2 <- rbindlist(pitchers_2)
setorder(pitchers_2, name)

# write.csv(pitchers_2, "pitchers-4-yrs.csv")
