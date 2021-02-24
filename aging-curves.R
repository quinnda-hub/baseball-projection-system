library(tidyverse)
library(janitor)
library(mgcv)
library(parallel)

# Set defaults 
season_min <- 1
pa_min <- 1


# Load data ; player stats from 2005 through 2019
players_raw <- read_csv("aging.csv")

players_tidy <- players_raw %>% 
  clean_names() %>%
  rename(woba = w_oba,
         wrc = w_rc) %>%
  mutate(hperpa = h/pa,
         x2bperpa = x2b/pa,
         x3bperpa = x3b/pa,
         hrperpa = hr/pa,
         sbperpa = sb/pa,
         csperpa = cs/pa,
         bbperpa = bb/pa,
         soperpa = so/pa,
         warperpa = war/pa) %>%
  select(-c(h, x2b, x3b, hr, sb, cs, bb, so, war))

### Centering around the mean ###
stat_sel <- function(df = players_tidy, stat, stat_denom = pa){
  
  stat <- enquo(stat)
  stat_denom <- enquo(stat_denom)
  
  df.1 <- df %>%
    group_by(season) %>%
    mutate(stat_sel = (!!stat) - weighted.mean((!!stat), (!!stat_denom))) %>%
    ungroup() %>%
    select(season, name, age, (!!stat), (!!stat_denom), stat_sel)
}
  
### Career stats functions ###
career_stats <- function(df, pa_min = 1, best = "high"){
  df.2 <- df %>%
    filter(pa >= pa_min[1])
  
  if(best == "high"){
    df.2 <- df.2 %>%
      group_by(name) %>%
      mutate(best_stat = max(stat_sel))
    
  } else {
    
    df.2 <- df.2 %>%
      group_by(name) %>%
      mutate(best_stat = min(stat_sel))
  }
  
  df.2 <- df.2 %>% mutate(
    first_year_mlb = min(season),
    first_year_age = ifelse(first_year_mlb == season, age, 0),
    last_year_mlb = max(season),
    last_year_age = ifelse(last_year_mlb == season, age, 0),
    stat_peak = ifelse(best_stat == stat_sel, 1, 0),
    peak_at_age = ifelse(stat_peak == 1, age, 0),
    lag_stat = lag(stat_sel),
    lag_pa = lag(pa),
    delta_stat = stat_sel - lag_stat) %>%
    summarise(seasons = n(),
              first_year_age = max(first_year_age),
              last_year_age = max(last_year_age),
              last_year_mlb = max(last_year_mlb),
              career_mean = weighted.mean(stat_sel, pa),
              career_pa = sum(pa),
              career_best_stat = max(best_stat),
              stat_age_peak = max(peak_at_age),
              mean_delta = mean(delta_stat, na.rm = TRUE), .groups = "drop")
  return(df.2)
}

### inter-player method ###
inter_player <- function(season_data, career_data, pa_min = 1, season_min = 1){
  
  df.3 <- career_data %>%
    left_join(season_data) %>%
    filter(pa >= pa_min[1]) %>%
    filter(seasons >= season_min[1]) %>%
    mutate(rel_age = age - stat_age_peak) %>% 
    select(season, name:stat_age_peak, stat_sel:rel_age, age, pa)
  
  return(df.3)
  
}

get_aging_models <- function(stat, df = players_tidy, stat_denom = pa){
  stat <- enquo(stat)
  stat_denom <- enquo(stat_denom)
  
  seasons <- stat_sel(df, !!stat, !!stat_denom)
  careers <- career_stats(seasons)
  inter_player <- inter_player(seasons, careers)
  
  years <- tibble(age = seq.int(21, 41))
  model <- gam(stat_sel ~ s(age) + career_mean,
               data = inter_player %>% filter(age >= 20 & age <= 41),
               weights = pa)
  
  years$result <- predict(model, newdata = years %>% mutate(
    career_mean = weighted.mean(inter_player$career_mean, inter_player$pa)
  ))
  
  return(years)
}

# Tried using the apply family of functions as well as looping
# and couldn't figure this out. Probably a solution staring me 
# in the face but this will do for now to keep going

set.seed(2020)

preds = c("hperpa", "x2bperpa", "x3bperpa", "hrperpa", "sbperpa",
          "csperpa", "bbperpa", "soperpa", "warperpa", "iso", "woba",
          "wrc")

aging_models <- list("hperpa" = get_aging_models(hperpa), 
                     "x2bperpa" = get_aging_models(x2bperpa),
                     "x3bperpa" = get_aging_models(x3bperpa), 
                     "hrperpa" = get_aging_models(hrperpa),
                     "sbperpa" = get_aging_models(sbperpa), 
                     "csperpa" = get_aging_models(csperpa),
                     "bbperpa" = get_aging_models(bbperpa), 
                     "soperpa" = get_aging_models(soperpa), 
                     "warperpa" = get_aging_models(warperpa), 
                     "iso" = get_aging_models(iso),
                     "woba" = get_aging_models(woba), 
                     "wrc" = get_aging_models(wrc))

save(aging_models, file = "aging-models")




