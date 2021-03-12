library(data.table)
library(janitor)

a_basic <- fread("a-standard-pitching-stats.csv")
a_advanced <- fread("a-advanced-pitching-stats.csv")
high_a_basic <- fread("a+-standard-pitching-stats.csv")
high_a_advanced <- fread("a+-advanced-pitching-stats.csv")
aa_basic <- fread("aa-standard-pitching-stats.csv")
aa_advanced <- fread("aa-advanced-pitching-stats.csv")

# cleaning the names
dfs <- list(a_basic, a_advanced, 
            high_a_basic, high_a_advanced,
            aa_basic, aa_advanced)

dfs <- lapply(dfs, clean_names)

a_basic <- dfs[[1]]
a_advanced <- dfs[[2]]
high_a_basic <- dfs[[3]]
high_a_advanced <- dfs[[4]]
aa_basic <- dfs[[5]]
aa_advanced <- dfs[[6]]

# changing "level" var to something useful
a_basic$level <- "a"
a_advanced$level <- "a"
high_a_basic$level <- "a+"
high_a_advanced$level <- "a+"
aa_basic$level <- "aa"
aa_advanced$level <- "aa"

basic <- rbind(a_basic, high_a_basic)
basic <- rbind(basic, aa_basic)
advanced <- rbind(a_advanced, high_a_advanced)
advanced <- rbind(advanced, aa_advanced)

sum_basic <- basic[, .(
             age = weighted.mean(age, ip), # weigh age by how many innings a 
             w = sum(w),                   # player threw at that age i.e. how 
             l = sum(l),                   # long a player stayed at that level
             g = sum(g),
             gs = sum(gs),
             ip = sum(ip),
             h = sum(h),
             r = sum(r),
             er = sum(er),
             hr = sum(hr),
             bb = sum(bb),
            so = sum(so)), by = name]

sum_advanced <- advanced[, .(
  fip = weighted.mean(fip, ip) # all we need from this table is "fip"
), by = name]

setorder(sum_basic, name)
setorder(sum_advanced, name)
sum_advanced$name <- NULL
total <- cbind(sum_basic, sum_advanced)

# write.csv(total, "minor-league-stats.csv")
