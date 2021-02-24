library(tidyverse)
library(tidymodels)

# This function grabs a specified baseball stat from a data frame, turns it into
# a rate state (if necessary) by dividing by PA, and then returns a vector 
# containing those stats. Most useful for building data frames used for
# regression and modeling. 
get_stat <- function(df, stat) {
  if (stat != "woba" & stat != "wrc+" & stat!= "g") {
    rate_stat <- df[[stat]] / df$pa
  } else {
    rate_stat <- df[[stat]]
  }
  return(rate_stat)
}

combined_minors <- read_csv("combined-minors.csv")

milb <- combined_minors %>% 
  arrange(player_id) 

id <- milb$player_id

# Filter and set PA limit for the first year in MLB for these players
mlb <- major_tidy %>%
  filter(player_id %in% id) %>%
  filter(pa >= 50) %>%
  filter(year_in_mlb == 1) %>% 
  arrange(player_id) 

id <- mlb$player_id
milb <- milb %>%
  filter(player_id %in% id)

# Filter and set PA limit for minor leaguers who had consecutive seasons in 
# the majors
milb.2 <- combined_minors %>%
  arrange(player_id)

id <- combined_minors$player_id

mlb.2 <- major_tidy %>%
  filter(player_id %in% id) %>%
  filter(pa >= 50) %>%
  group_by(name) %>%
  filter(1 %in% year_in_mlb & 2 %in% year_in_mlb) %>%
  filter(year_in_mlb < 3) %>%
  arrange(player_id)

id <- mlb.2$player_id
id <- id[seq(1, length(id), 2)]
milb.2 <- milb.2 %>%
  filter(player_id %in% id) 

mlb.21 <- mlb.2 %>%
  filter(year_in_mlb == 1)

mlb.22 <- mlb.2 %>% 
  filter(year_in_mlb == 2)

# Filter and set PA limit for minor leaguers who have had three consecutive
# seasons in the majors
milb.3 <- combined_minors %>%
  arrange(player_id)
id <- combined_minors$player_id

mlb.3 <- major_tidy %>%
  filter(player_id %in% id) %>%
  filter(pa >= 50) %>%
  group_by(name) %>%
  filter(1 %in% year_in_mlb & 2 %in% year_in_mlb & 3 %in% year_in_mlb) %>%
  filter(year_in_mlb < 4) %>%
  arrange(player_id)

id <- mlb.3$player_id
id <- id[seq(1, length(id), 3)]
milb.3 <- milb.3 %>%
  filter(player_id %in% id)

mlb.31 <- mlb.3 %>%
  filter(year_in_mlb == 1)

mlb.32 <- mlb.3 %>% 
  filter(year_in_mlb == 2)

mlb.33 <- mlb.3 %>% 
  filter(year_in_mlb == 3)

# Filter and set PA limit for minor leaguers who have had four consecutive 
# seasons in the majors
milb.4 <- combined_minors %>%
  arrange(player_id)
id <- combined_minors$player_id

mlb.4 <- major_tidy %>%
  filter(player_id %in% id) %>%
  filter(pa >= 50) %>%
  group_by(name) %>%
  filter(1 %in% year_in_mlb & 2 %in% year_in_mlb &
           3 %in% year_in_mlb & 4 %in% year_in_mlb) %>%
  filter(year_in_mlb < 5) %>%
  arrange(player_id)

id <- mlb.4$player_id
id <- id[seq(1, length(id), 4)]
milb.4 <- milb.4 %>%
  filter(player_id %in% id)

mlb.41 <- mlb.4 %>%
  filter(year_in_mlb == 1)

mlb.42 <- mlb.4 %>% 
  filter(year_in_mlb == 2)

mlb.43 <- mlb.4 %>% 
  filter(year_in_mlb == 3)

mlb.44 <- mlb.4 %>% 
  filter(year_in_mlb == 4)

# Create the table for regression for players with no major league experience
reg_1 <- tibble(hperpa_milb = get_stat(milb, "h"),
                     x2bperpa_milb = get_stat(milb, "x2b"),
                     x3bperpa_milb = get_stat(milb, "x3b"),
                     hrperpa_milb = get_stat(milb, "hr"),
                     rbiperpa_milb = get_stat(milb, "rbi"),
                     bbperpa_milb = get_stat(milb, "bb"),
                     soperpa_milb = get_stat(milb, "so"),
                     hbpperpa_milb = get_stat(milb, "hbp"),
                     sfperpa_milb = get_stat(milb, "sf"),
                     shperpa_milb = get_stat(milb, "sh"),
                     gidpperpa_milb = get_stat(milb, "gidp"),
                     sbperpa_milb = get_stat(milb, "sb"),
                     csperpa_milb = get_stat(milb, "cs"),
                     woba_milb = get_stat(milb, "woba"),
                     `wrc+_milb` = get_stat(milb, "wrc+"),
                     g_milb = get_stat(milb, "g"),
                     age_milb = milb$final_age,
                     hperpa_year1 = get_stat(mlb, "h"),
                     x2bperpa_year1 = get_stat(mlb, "x2b"),
                     x3bperpa_year1 = get_stat(mlb, "x3b"),
                     hrperpa_year1 = get_stat(mlb, "hr"),
                     rbiperpa_year1 = get_stat(mlb, "rbi"),
                     bbperpa_year1 = get_stat(mlb, "bb"),
                     soperpa_year1 = get_stat(mlb, "so"),
                     hbpperpa_year1 = get_stat(mlb, "hbp"),
                     sfperpa_year1 = get_stat(mlb, "sf"),
                     shperpa_year1 = get_stat(mlb, "sh"),
                     gidpperpa_year1 = get_stat(mlb, "gidp"),
                     sbperpa_year1 = get_stat(mlb, "sb"),
                     csperpa_year1 = get_stat(mlb, "cs"),
                     woba_year1 = get_stat(mlb, "woba"),
                     g_year1 = mlb$g,
                     pa_year1 = mlb$pa,
                     `wrc+_year1` = get_stat(mlb, "wrc+"),
                     warperpa_year1 = get_stat(mlb, "war"))

# Create regression table for players with 1 year of MLB experience
reg_2 <- tibble(hperpa_milb = get_stat(milb.2, "h"),
                x2bperpa_milb = get_stat(milb.2, "x2b"),
                x3bperpa_milb = get_stat(milb.2, "x3b"),
                hrperpa_milb = get_stat(milb.2, "hr"),
                rbiperpa_milb = get_stat(milb.2, "rbi"),
                bbperpa_milb = get_stat(milb.2, "bb"),
                soperpa_milb = get_stat(milb.2, "so"),
                hbpperpa_milb = get_stat(milb.2, "hbp"),
                sfperpa_milb = get_stat(milb.2, "sf"),
                shperpa_milb = get_stat(milb.2, "sh"),
                gidpperpa_milb = get_stat(milb.2, "gidp"),
                sbperpa_milb = get_stat(milb.2, "sb"),
                csperpa_milb = get_stat(milb.2, "cs"),
                woba_milb = get_stat(milb.2, "woba"),
                `wrc+_milb` = get_stat(milb.2, "wrc+"),
                g_milb = get_stat(milb.2, "g"),
                age_milb = milb.2$final_age,
                hperpa_year1 = get_stat(mlb.21, "h"),
                x2bperpa_year1 = get_stat(mlb.21, "x2b"),
                x3bperpa_year1 = get_stat(mlb.21, "x3b"),
                hrperpa_year1 = get_stat(mlb.21, "hr"),
                rbiperpa_year1 = get_stat(mlb.21, "rbi"),
                bbperpa_year1 = get_stat(mlb.21, "bb"),
                soperpa_year1 = get_stat(mlb.21, "so"),
                hbpperpa_year1 = get_stat(mlb.21, "hbp"),
                sfperpa_year1 = get_stat(mlb.21, "sf"),
                shperpa_year1 = get_stat(mlb.21, "sh"),
                gidpperpa_year1 = get_stat(mlb.21, "gidp"),
                sbperpa_year1 = get_stat(mlb.21, "sb"),
                csperpa_year1 = get_stat(mlb.21, "cs"),
                woba_year1 = get_stat(mlb.21, "woba"),
                `wrc+_year1` = get_stat(mlb.21, "wrc+"),
                warperpa_year1 = get_stat(mlb.21, "war"),
                g_year1 = mlb.21$g,
                pa_year1 = mlb.21$pa,
                hperpa_year2 = get_stat(mlb.22, "h"),
                x2bperpa_year2 = get_stat(mlb.22, "x2b"),
                x3bperpa_year2 = get_stat(mlb.22, "x3b"),
                hrperpa_year2 = get_stat(mlb.22, "hr"),
                rbiperpa_year2 = get_stat(mlb.22, "rbi"),
                bbperpa_year2 = get_stat(mlb.22, "bb"),
                soperpa_year2 = get_stat(mlb.22, "so"),
                hbpperpa_year2 = get_stat(mlb.22, "hbp"),
                sfperpa_year2 = get_stat(mlb.22, "sf"),
                shperpa_year2 = get_stat(mlb.22, "sh"),
                gidpperpa_year2 = get_stat(mlb.22, "gidp"),
                sbperpa_year2 = get_stat(mlb.22, "sb"),
                csperpa_year2 = get_stat(mlb.22, "cs"),
                woba_year2 = get_stat(mlb.22, "woba"),
                `wrc+_year2` = get_stat(mlb.22, "wrc+"),
                warperpa_year2 = get_stat(mlb.22, "war"),
                g_year2 = mlb.22$g,
                pa_year2 = mlb.22$pa,
                )

# Create regression table for those players with 2 years of MLB experience
reg_3 <- tibble(hperpa_milb = get_stat(milb.3, "h"),
                x2bperpa_milb = get_stat(milb.3, "x2b"),
                x3bperpa_milb = get_stat(milb.3, "x3b"),
                hrperpa_milb = get_stat(milb.3, "hr"),
                rbiperpa_milb = get_stat(milb.3, "rbi"),
                bbperpa_milb = get_stat(milb.3, "bb"),
                soperpa_milb = get_stat(milb.3, "so"),
                hbpperpa_milb = get_stat(milb.3, "hbp"),
                sfperpa_milb = get_stat(milb.3, "sf"),
                shperpa_milb = get_stat(milb.3, "sh"),
                gidpperpa_milb = get_stat(milb.3, "gidp"),
                sbperpa_milb = get_stat(milb.3, "sb"),
                csperpa_milb = get_stat(milb.3, "cs"),
                woba_milb = get_stat(milb.3, "woba"),
                `wrc+_milb` = get_stat(milb.3, "wrc+"),
                g_milb = get_stat(milb.3, "g"),
                age_milb = milb.3$final_age,
                hperpa_year1 = get_stat(mlb.31, "h"),
                x2bperpa_year1 = get_stat(mlb.31, "x2b"),
                x3bperpa_year1 = get_stat(mlb.31, "x3b"),
                hrperpa_year1 = get_stat(mlb.31, "hr"),
                rbiperpa_year1 = get_stat(mlb.31, "rbi"),
                bbperpa_year1 = get_stat(mlb.31, "bb"),
                soperpa_year1 = get_stat(mlb.31, "so"),
                hbpperpa_year1 = get_stat(mlb.31, "hbp"),
                sfperpa_year1 = get_stat(mlb.31, "sf"),
                shperpa_year1 = get_stat(mlb.31, "sh"),
                gidpperpa_year1 = get_stat(mlb.31, "gidp"),
                sbperpa_year1 = get_stat(mlb.31, "sb"),
                csperpa_year1 = get_stat(mlb.31, "cs"),
                woba_year1 = get_stat(mlb.31, "woba"),
                `wrc+_year1` = get_stat(mlb.31, "wrc+"),
                warperpa_year1 = get_stat(mlb.31, "war"),
                g_year1 = mlb.31$g,
                pa_year1 = mlb.31$pa,
                hperpa_year2 = get_stat(mlb.32, "h"),
                x2bperpa_year2 = get_stat(mlb.32, "x2b"),
                x3bperpa_year2 = get_stat(mlb.32, "x3b"),
                hrperpa_year2 = get_stat(mlb.32, "hr"),
                rbiperpa_year2 = get_stat(mlb.32, "rbi"),
                bbperpa_year2 = get_stat(mlb.32, "bb"),
                soperpa_year2 = get_stat(mlb.32, "so"),
                hbpperpa_year2 = get_stat(mlb.32, "hbp"),
                sfperpa_year2 = get_stat(mlb.32, "sf"),
                shperpa_year2 = get_stat(mlb.32, "sh"),
                gidpperpa_year2 = get_stat(mlb.32, "gidp"),
                sbperpa_year2 = get_stat(mlb.32, "sb"),
                csperpa_year2 = get_stat(mlb.32, "cs"),
                woba_year2 = get_stat(mlb.32, "woba"),
                `wrc+_year2` = get_stat(mlb.32, "wrc+"),
                warperpa_year2 = get_stat(mlb.32, "war"),
                g_year2 = mlb.32$g,
                pa_year2 = mlb.32$pa,
                hperpa_year3 = get_stat(mlb.33, "h"),
                x2bperpa_year3 = get_stat(mlb.33, "x2b"),
                x3bperpa_year3 = get_stat(mlb.33, "x3b"),
                hrperpa_year3 = get_stat(mlb.33, "hr"),
                rbiperpa_year3 = get_stat(mlb.33, "rbi"),
                bbperpa_year3 = get_stat(mlb.33, "bb"),
                soperpa_year3 = get_stat(mlb.33, "so"),
                hbpperpa_year3 = get_stat(mlb.33, "hbp"),
                sfperpa_year3 = get_stat(mlb.33, "sf"),
                shperpa_year3 = get_stat(mlb.33, "sh"),
                gidpperpa_year3 = get_stat(mlb.33, "gidp"),
                sbperpa_year3 = get_stat(mlb.33, "sb"),
                csperpa_year3 = get_stat(mlb.33, "cs"),
                woba_year3 = get_stat(mlb.33, "woba"),
                `wrc+_year3` = get_stat(mlb.33, "wrc+"),
                warperpa_year3 = get_stat(mlb.33, "war"),
                g_year3 = mlb.33$g,
                pa_year3 = mlb.33$pa)

### Create models for players with no MLB experience ###
get_model <- function(stat, df, end_of_predictor) {
  end <- end_of_predictor
  vector <- df %>%
    pull(stat)
  
  df <- df %>%
    select(-(ends_with(end))) %>%
    mutate(response = vector)
  
  stat_split <- initial_split(df)
  stat_train <- training(stat_split)
  stat_test <- testing(stat_split)
  
  stat_rec <- recipe(response ~ ., data = df) %>%
    step_zv(all_numeric(), -all_outcomes()) %>%
    step_normalize(all_numeric(), -all_outcomes())
  
  stat_prep <- stat_rec %>%
    prep()
  
  stat_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
    set_engine("glmnet")
  
  stat_wf <- workflow() %>%
    add_recipe(stat_rec)
  
  stat_fit <- stat_wf %>%
    add_model(stat_spec) %>%
    fit(data = stat_train)
  
  stat_boot <- bootstraps(stat_train)
  
  stat_tune <- linear_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")
  
  stat_grid <- grid_regular(penalty(), levels = 50)
  
  lasso_grid <- tune_grid(
    stat_wf %>% add_model(stat_tune),
    resamples = stat_boot,
    grid = stat_grid)
  
  lowest_rmse <- lasso_grid %>% 
    select_best("rmse")
  
  final_stat <- finalize_workflow(stat_wf %>% add_model(stat_tune),
                                 lowest_rmse)
  
  final_fit <- final_stat %>%
    fit(stat_train)
}

resp_1 <- c("hperpa_year1", "x2bperpa_year1", "x3bperpa_year1",
          "hrperpa_year1", "bbperpa_year1",
          "soperpa_year1", "hbpperpa_year1", "sfperpa_year1",
          "shperpa_year1", "gidpperpa_year1", "sbperpa_year1",
          "csperpa_year1", "woba_year1", "wrc+_year1", "g_year1",
          "pa_year1", "warperpa_year1")

resp_2 <- c("hperpa_year2", "x2bperpa_year2", "x3bperpa_year2",
          "hrperpa_year2", "bbperpa_year2",
          "soperpa_year2", "hbpperpa_year2", "sfperpa_year2",
          "shperpa_year2", "gidpperpa_year2", "sbperpa_year2",
          "csperpa_year2", "woba_year2", "wrc+_year2", "g_year2",
          "pa_year2", "warperpa_year2")

resp_3 <- c("hperpa_year3", "x2bperpa_year3", "x3bperpa_year3",
            "hrperpa_year3", "bbperpa_year3",
            "soperpa_year3", "hbpperpa_year3", "sfperpa_year3",
            "shperpa_year3", "gidpperpa_year3", "sbperpa_year3",
            "csperpa_year3", "woba_year3", "wrc+_year3", "g_year3",
            "pa_year3", "warperpa_year3")

library(parallel)
set.seed(2020)
milb.1_models <- mclapply(resp_1, get_model, df = reg_1, end_of_predictor = "year1")
milb.2_models <- mclapply(resp_2, get_model, df = reg_2, end_of_predictor = "year2")
milb.3_models <- mclapply(resp_3, get_model, df = reg_3, end_of_predictor = "year3")

save(milb.1_models, milb.2_models, milb.3_models, file = "minor-league-models")









