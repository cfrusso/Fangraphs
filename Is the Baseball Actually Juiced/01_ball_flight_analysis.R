setwd("~/Statcast/Yearly Data")

load("pbp2019.rda")
load("pbp2018.rda")
load("pbp2017.rda")
load("pbp2016.rda")
load("pbp2015.rda")

library(dplyr)
library(randomForest)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

pbp2019 <- pbp2019 %>%
  mutate(launch_speed = as.numeric(launch_speed),
         launch_angle = as.numeric(launch_angle),
         hit_distance_sc = as.numeric(hit_distance_sc))
pbp2018 <- pbp2018 %>%
  mutate(launch_speed = as.numeric(launch_speed),
         launch_angle = as.numeric(launch_angle),
         hit_distance_sc = as.numeric(hit_distance_sc))
pbp2017 <- pbp2017 %>%
  mutate(launch_speed = as.numeric(launch_speed),
         launch_angle = as.numeric(launch_angle),
         hit_distance_sc = as.numeric(hit_distance_sc))
pbp2016 <- pbp2016 %>%
  mutate(launch_speed = as.numeric(launch_speed),
         launch_angle = as.numeric(launch_angle),
         hit_distance_sc = as.numeric(hit_distance_sc))
pbp2015 <- pbp2015 %>%
  mutate(launch_speed = as.numeric(launch_speed),
         launch_angle = as.numeric(launch_angle),
         hit_distance_sc = as.numeric(hit_distance_sc))


# fly balls

flys_2019 <- filter(pbp2019, bb_type == "fly_ball",
                    month(as.Date(game_date,"%m/%d/%Y")) <= 6)
flys_2018 <- filter(pbp2018, bb_type == "fly_ball",
                    month(as.Date(game_date,"%m/%d/%Y")) <= 6)
flys_2017 <- filter(pbp2017, bb_type == "fly_ball",
                    month(as.Date(game_date,"%m/%d/%Y")) <= 6)
flys_2016 <- filter(pbp2016, bb_type == "fly_ball",
                    month(as.Date(game_date,"%m/%d/%Y")) <= 6)
flys_2015 <- filter(pbp2015, bb_type == "fly_ball",
                    month(as.Date(game_date,"%m/%d/%Y")) <= 6)


test <- data.frame(launch_speed = c(70,70,70,70,70,70,
                                    80,80,80,80,80,80,
                                    90,90,90,90,90,90,
                                    100,100,100,100,100,100),
                   launch_angle = c(20,25,30,35,40,45,
                                    20,25,30,35,40,45,
                                    20,25,30,35,40,45,
                                    20,25,30,35,40,45))

set.seed(1234)
rf_2019 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = flys_2019, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2018 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = flys_2018, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2017 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = flys_2017, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2016 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = flys_2016, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2015 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = flys_2015, ntree=100,importance=TRUE, na.action = na.omit)


results_2019 <- predict(rf_2019, test)
results_2018 <- predict(rf_2018, test)
results_2017 <- predict(rf_2017, test)
results_2016 <- predict(rf_2016, test)
results_2015 <- predict(rf_2015, test)


results_all <- data.frame(dist_19 = results_2019,
                          dist_18 = results_2018,
                          dist_17 = results_2017,
                          dist_16 = results_2016,
                          dist_15 = results_2015,
                          launch_speed = test$launch_speed,
                          launch_angle = test$launch_angle)

results_long <- gather(results_all, year, distance, dist_19:dist_15) %>%
  mutate(Year = factor(ifelse(year == "dist_19",2019,
                              ifelse(year == "dist_18",2018,
                                     ifelse(year == "dist_17",2017,
                                            ifelse(year == "dist_16",2016,2015))))))


fly_plot <- function(df,mph){
  ggplot(df, aes(x = launch_angle, y = distance, col = Year)) +
    geom_point(size = 5) +
    scale_color_manual(breaks = c("2019","2018","2017","2016","2015"),
                       values = c("darkorange","orange","lightblue",
                                  "cornflowerblue","blue")) +
    ggtitle(paste0("Exit Velocity: ",as.character(mph)," mph")) +
    xlab("Launch Angle") +
    ylab("Distance") +
    theme_minimal() +
    border()
}

velo_70 <- results_long %>% filter(launch_speed == 70)
velo_80 <- results_long %>% filter(launch_speed == 80)
velo_90 <- results_long %>% filter(launch_speed == 90)
velo_100 <- results_long %>% filter(launch_speed == 100)

plot_70 <- fly_plot(velo_70,70)
plot_80 <- fly_plot(velo_80,80)
plot_90 <- fly_plot(velo_90,90)
plot_100 <- fly_plot(velo_100,100)


ggarrange(plot_100,plot_90,plot_80,plot_70,ncol=2,nrow=2,
          common.legend = T, legend="right")


# homers

homers_2019 <- filter(pbp2019, events == "home_run",
                      month(as.Date(game_date,"%m/%d/%Y")) <= 6)
homers_2018 <- filter(pbp2018, events == "home_run",
                      month(as.Date(game_date,"%m/%d/%Y")) <= 6)
homers_2017 <- filter(pbp2017, events == "home_run",
                      month(as.Date(game_date,"%m/%d/%Y")) <= 6)
homers_2016 <- filter(pbp2016, events == "home_run",
                      month(as.Date(game_date,"%m/%d/%Y")) <= 6)
homers_2015 <- filter(pbp2015, events == "home_run",
                      month(as.Date(game_date,"%m/%d/%Y")) <= 6)

test <- data.frame(launch_speed = c(90,90,90,90,
                                    100,100,100,100,
                                    110,110,110,110),
                   launch_angle = c(20,25,30,35,
                                    20,25,30,35,
                                    20,25,30,35))

set.seed(1234)
rf_2019 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = homers_2019, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2018 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = homers_2018, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2017 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = homers_2017, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2016 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = homers_2016, ntree=100,importance=TRUE, na.action = na.omit)
set.seed(1234)
rf_2015 <- randomForest(hit_distance_sc ~ launch_speed + launch_angle,
                        data = homers_2015, ntree=100,importance=TRUE, na.action = na.omit)


results_2019 <- predict(rf_2019, test)
results_2018 <- predict(rf_2018, test)
results_2017 <- predict(rf_2017, test)
results_2016 <- predict(rf_2016, test)
results_2015 <- predict(rf_2015, test)

results_all <- data.frame(dist_19 = results_2019,
                          dist_18 = results_2018,
                          dist_17 = results_2017,
                          dist_16 = results_2016,
                          dist_15 = results_2015,
                          launch_speed = test$launch_speed,
                          launch_angle = test$launch_angle)

results_long <- gather(results_all, year, distance, dist_19:dist_15) %>%
  mutate(Year = factor(ifelse(year == "dist_19",2019,
                              ifelse(year == "dist_18",2018,
                                     ifelse(year == "dist_17",2017,
                                            ifelse(year == "dist_16",2016,2015))))))


fly_plot_alt <- function(df,mph){
  ggplot(df, aes(x = launch_angle, y = distance, col = Year)) +
    geom_point(size = 5) +
    scale_color_manual(breaks = c("2019","2018","2017","2016","2015"),
                       values = c("darkorange","orange","lightblue",
                                  "cornflowerblue","blue")) +
    ggtitle(paste0("Exit Velocity: ",as.character(mph)," mph")) +
    ylab("Distance") +
    theme_minimal() +
    border() +
    theme(plot.title = element_text(size = 10),
          axis.title.x = element_blank())
}


velo_90 <- results_long %>% filter(launch_speed == 90)
velo_100 <- results_long %>% filter(launch_speed == 100)
velo_110 <- results_long %>% filter(launch_speed == 110)

plot_90 <- fly_plot(velo_90,90)
plot_100 <- fly_plot(velo_100,100)
plot_110 <- fly_plot(velo_110,110)

ggarrange(plot_110,plot_100,plot_90,ncol=1,nrow=3,
          common.legend = T, legend="right")








