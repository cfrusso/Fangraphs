setwd("~/Attendence By Pitcher")

library(dplyr)
library(lubridate)
library(randomForest)

load("logs.rda")
logs$date <- as.Date(as.character(logs$date), format = "%Y%m%d")
stats <- read.csv("pitcher_stats.csv", stringsAsFactors = F)
ids <- read.csv("ids_full.csv", stringsAsFactors = F)

get_diffs <- function(yr){
  
  # filter games
  logs_sub <- logs %>% filter(year(date) == yr)

  # data for model
  games <- logs_sub %>%
    mutate(starter = as.character(h_starting_pitcher_id),
           team = factor(h_name),
           month = factor(month(date)),
           day_type = factor(ifelse(day_of_week %in% c("Fri","Sat","Sun"),
                                    "Weekend","Weekday")),
           time_of_day = factor(day_night)) %>%
    filter(attendance > 0) %>%
    select(attendance, starter, team, month, day_type, time_of_day)
  
  # build model
  set.seed(1234)
  fit.forest <- randomForest(attendance ~ . -starter,
                             data = games,
                             ntree=100,
                             importance=TRUE)
  
  # differentials
  games$pred <- predict(fit.forest, games)
  games$diff <- (games$attendance - games$pred) / games$pred * 100

  # diff by starter
  differentials <- games %>%
    group_by(starter) %>%
    summarize(avg_diff = mean(diff),
              starts = n()) %>%
    filter(starts >= 10) %>%
    arrange(desc(avg_diff))

  # pitcher stats
  stats_sub <- filter(stats, Season == yr)
  
  # final table
  compare <- differentials %>%
    mutate(key_retro = starter) %>%
    left_join(ids, by = "key_retro") %>%
    mutate(playerid = key_fangraphs) %>%
    left_join(stats_sub, by = "playerid") %>%
    select(Name, WAR, avg_diff) %>%
    na.omit()
  

  # group pitchers
  cuts <- quantile(compare$WAR, probs = c(.1,.5,.9))
  compare$war_range <- factor(ifelse(compare$WAR >= cuts[3], "Best",
                                     ifelse(compare$WAR >= cuts[2], "Above_avg",
                                            ifelse(compare$WAR >= cuts[1], "Below_avg","Worst"))))
 
  # diff by group
  diffs <- by(compare$avg_diff, compare$war_range, mean)
  df <- data.frame(Best = diffs[3], Above_avg = diffs[1], Below_avg = diffs[2], Worst = diffs[4])
  rownames(df) <- yr
  
  # model performance
  df$model_Rsquared <- cor(games$attendance, games$pred)^2
  
  # correlation between pitcher WAR and differential
  y <- cor.test(compare$WAR, compare$avg_diff)
  df$diff_WAR_cor <- y$estimate
  df$diff_WAR_pval <- y$p.value
  
  # number in each group
  num <- table(compare$war_range)
  df$Above_avg_num <- num[1]
  df$Below_avg_num <- num[2]
  df$Best_num <- num[3]
  df$Worst_num <- num[4]
  
  df
}



results <- get_diffs(1935)
for(yr in 1936:2018){
  results <- rbind(results, get_diffs(yr))
}
results$year <- rownames(results)
rownames(results) <- NULL

save(results, file = "results.rda")

