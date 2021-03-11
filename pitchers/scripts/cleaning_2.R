library(data.table)
# this function transforms the data into a form fit for linear regression
reg_df <- function(df, vars, yrs, denom) {
  df_1 <- df
  # helper function that grabs that stats per year divided by the 
  # specified denominator
  rate_stat <- function(df, stat, year, yrs, denom) {
    rate_stats <- c("babip", "woba", "wrc+", "g", "gs")
    df <- df[seq(year, nrow(df), yrs), ]
    if (stat == "ip") {
      df[[stat]] / df$gs
    } else if (stat == "pa") {
      df[[stat]] / df$g
    }  else if (stat %in% rate_stats) {
      df[[stat]]
    }  else {
      df[[stat]] / df[[denom]]
    }
  }
  
  # create the names for the columns
  names <- vector("list", yrs) 
  
  for (i in seq_along(1:yrs)) {
    names[i] <- list(paste0(vars, "per", denom, "_year", i))
  }

  # flatten the names list
  names <- unlist(names)
  
  # create list to fill with the values from rate_stat
  values <- vector("list", yrs)
  # iterate through number of years to calc values for each year
  # and store it in aforementioned list "values"
  for (yr in seq_along(1:yrs)) {
    vals <- sapply(vars, rate_stat, df = df_1, year = yr, yrs = yrs, denom = denom)  
    values[[yr]] <- vals
  }

  df_2 <- as.data.table(cbind.data.frame(values))
  
  names(df_2) <- names
  
  # this is very hack-y but it works
  rep_names <- function(df, yrs) {
    if ("ipperip_year1" %in% colnames(df)) {
      old_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        old_names[i] <- list(paste0("ipperip", "_year", i))
      }
      
      new_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        new_names[i] <- list(paste0("ippergs", "_year", i))
      }
      
      setnames(df, unlist(old_names), unlist(new_names))
    }
    
    if ("paperg_year1" %in% colnames(df)) {
      old_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        old_names[i] <- list(paste0("paperpa", "_year", i))
      }
      
      new_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        new_names[i] <- list(paste0("paperg", "_year", i))
      }
      
      setnames(df, unlist(old_names), unlist(new_names))
    }
    
    if ("babipperip_year1" %in% colnames(df)) {
      old_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        old_names[i] <- list(paste0("babipperip", "_year", i))
      }
      
      new_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        new_names[i] <- list(paste0("babip", "_year", i))
      }
      
      setnames(df, unlist(old_names), unlist(new_names))
    }
    
    if ("wobaperpa_year1" %in% colnames(df)) {
      old_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        old_names[i] <- list(paste0("wobaperpa", "_year", i))
      }
      
      new_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        new_names[i] <- list(paste0("woba", "_year", i))
      }
      
      setnames(df, unlist(old_names), unlist(new_names))
    }
    
    if ("wrc+perpa_year1" %in% colnames(df)) {
      old_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        old_names[i] <- list(paste0("wrc+perpa", "_year", i))
      }
      
      new_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        new_names[i] <- list(paste0("wrc+", "_year", i))
      }
      
      setnames(df, unlist(old_names), unlist(new_names))
    }
    
    if ("gsperip_year1" %in% colnames(df)) {
      old_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        old_names[i] <- list(paste0("gsperip", "_year", i))
      }
      
      new_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        new_names[i] <- list(paste0("gs", "_year", i))
      }
      
      setnames(df, unlist(old_names), unlist(new_names))
    }
    
    if ("gperpa_year1" %in% colnames(df)) {
      old_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        old_names[i] <- list(paste0("gperpa", "_year", i))
      }
      
      new_names <- vector("list", yrs)
      
      for (i in seq_along(1:yrs)) {
        new_names[i] <- list(paste0("g", "_year", i))
      }
      
      setnames(df, unlist(old_names), unlist(new_names))
    }
  }
  
  rep_names(df_2, yrs)
  
  return (df_2)
}

pitchers_5_yrs <- read.csv("pitchers-5-yrs.csv")
pitchers_4_yrs <- read.csv("pitchers-4-yrs.csv")

# create regression table for pitchers with 5 & 4 consecutive years
vars <- c("w", "l", "gs", "ip", "babip", "h",
          "r", "er", "hr", "bb", "so", "fip", "war")

reg_5_yrs <- reg_df(pitchers_5_yrs, vars = vars, 5, "ip")
reg_4_yrs <- reg_df(pitchers_4_yrs, vars = vars, 4, "ip")
