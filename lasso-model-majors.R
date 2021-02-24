library(tidyverse)
library(tidymodels)
library(parallel)

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

set.seed(2020)

resp_1 <- c("hperpa_year5", "x2bperpa_year5", "x3bperpa_year5",
             "hrperpa_year5", "sbperpa_year5", "csperpa_year5",
             "bbperpa_year5", "soperpa_year5", "hbpperpa_year5",
             "shperpa_year5", "sfperpa_year5", "gidpperpa_year5",
            "woba_year5", "wrc_year5", "warperpa_year5", 
            "papergame_year5", "g_year5")

reg_2 <- reg %>% select(-c("hperpa_year5", "x2bperpa_year5", "x3bperpa_year5",
                           "hrperpa_year5", "sbperpa_year5", "csperpa_year5",
                           "bbperpa_year5", "soperpa_year5", "hbpperpa_year5",
                           "shperpa_year5", "sfperpa_year5", "gidpperpa_year5",
                           "woba_year5", "wrc_year5", "warperpa_year5", 
                           "papergame_year5", "g_year5", "rbiperpa_year5"))

resp_2 <- c("hperpa_year4", "x2bperpa_year4", "x3bperpa_year4",
"hrperpa_year4", "sbperpa_year4", "csperpa_year4",
"bbperpa_year4", "soperpa_year4", "hbpperpa_year4",
"shperpa_year4", "sfperpa_year4", "gidpperpa_year4",
"woba_year4", "wrc_year4", "warperpa_year4", 
"papergame_year4", "g_year4")

ml_year4_model <- mclapply(resp_1, get_model, df = reg,
                           end_of_predictor = "year5")

ml_year3_model <- mclapply(resp_2, get_model, df = reg_2,
                           end_of_predictor = "year4")


save(ml_year4_model, ml_year3_model, file = "major-league-models")
