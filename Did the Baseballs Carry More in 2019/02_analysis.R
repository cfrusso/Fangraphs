
setwd("~/Fangraphs Blog/08_BallFlightFull")

library(tidyverse)
library(lubridate)
library(randomForest)
library(gridExtra)

# function for plotting
plot_diff <- function(diff){
  mid = signif(median(diff),3)
  avg =  signif(mean(diff),3)
  first =  signif(quantile(diff,0.25),3)
  third =  signif(quantile(diff,0.75),3)
  max = max(diff)
  ggplot(mapping = aes(diff)) +
    geom_density() +
    geom_vline(aes(xintercept = avg),
               color = "darkorange1", size = 1) +
    geom_vline(aes(xintercept = mid),
               color = "blue", size = 1) +
    geom_vline(aes(xintercept = first),
               color = "forestgreen", size = 1) +
    geom_vline(aes(xintercept = third),
               color = "forestgreen", size = 1) +
    geom_text(aes(x = max*.7, label = paste("1st Quartile:",first), y = 0.016),
              color = "forestgreen", size = 5) +
    geom_text(aes(x = max*.7, label = paste("Median:",mid), y = 0.014),
              color = "blue", size = 5) +
    geom_text(aes(x = max*.7, label = paste("Mean:",avg), y = 0.012),
              color = "darkorange1", size = 5) +
    geom_text(aes(x = max*.7, label = paste("3rd Quartile:",third), y = 0.010),
              color = "forestgreen", size = 5) +
    labs(x = "Differential") +
           theme_minimal()
}

load("flight.rda")

# filter for well struck balls in the air
flight <- flight %>%
  filter(launch_angle <= 45, launch_angle >= 10, launch_speed >= 70)

# regular season
reg <- flight %>%
  filter(postseason == 0)

# by year
reg %>%
  mutate(Year = game_year) %>%
  group_by(Year) %>%
  summarize(Observations = n(),
            "Exit Velocity" = mean(launch_speed),
            "Launch Angle" = mean(launch_angle),
            Distance = mean(hit_distance_sc, na.rm = T))

# by type
reg %>%
  group_by(game_year, bb_type) %>%
  summarize(Observations = n(),
            "Exit Velocity" = mean(launch_speed),
            "Launch Angle" = mean(launch_angle),
            Distance = mean(hit_distance_sc, na.rm = T)) %>%
  arrange(bb_type)

# homers
reg %>%
  filter(events == "home_run") %>%
  group_by(game_year) %>%
  summarize(Observations = n(),
            "Exit Velocity" = mean(launch_speed),
            "Launch Angle" = mean(launch_angle),
            Distance = mean(hit_distance_sc, na.rm = T))

# prepare data
cleaned <- flight %>%
  mutate(home_team = factor(ifelse(home_team == "ATL" & game_year < 2017, "ATL_prev", home_team)),
         month = ifelse(month(game_date) < 4, 4,
                        ifelse(month(game_date) > 9, 9, month(game_date)))) %>%
  select(hit_distance_sc, launch_speed, launch_angle, home_team, month, game_year, postseason) %>%
  na.omit()

reg_2015_2018 <- cleaned %>%
  filter(game_year < 2019, postseason == 0) %>%
  select(-game_year,-postseason)

reg_2019 <- cleaned %>%
  filter(game_year == 2019, postseason == 0) %>%
  select(-game_year,-postseason)

# split 2015-2018 into train/test
set.seed(1234)
rand_order <- sample(1:nrow(reg_2015_2018))
split <- floor(.8*nrow(reg_2015_2018))
index_train <- rand_order < split
index_test <- rand_order >= split
train <- reg_2015_2018[index_train,]
test <- reg_2015_2018[index_test,]

# build model
set.seed(1234)
fit_reg_2015_2018 <- randomForest(hit_distance_sc ~ ., data = train,
                                  ntree = 10,
                                  importance = TRUE)

# check fit on test
pred <- predict(fit_reg_2015_2018, test)
y <- test$hit_distance_sc
1 - sum((y-pred)^2)/sum((y-mean(y))^2)

# variable importance
varImpPlot(fit_reg_2015_2018, type = 1)

# 2019
pred_2019 <- predict(fit_reg_2015_2018, reg_2019)
y <- reg_2019$hit_distance_sc
1 - sum((y-pred_2019)^2)/sum((y-mean(y))^2)

diff_reg <- reg_2019$hit_distance_sc - pred_2019
plot_diff(diff_reg)


# 2019 reg vs postseason
flight %>%
  filter(game_year == 2019) %>%
  group_by(postseason) %>%
  summarize(Observations = n(),
            "Exit Velocity" = mean(launch_speed),
            "Launch Angle" = mean(launch_angle),
            Distance = mean(hit_distance_sc, na.rm = T))

# by type
flight %>%
  filter(game_year == 2019) %>%
  group_by(postseason, bb_type) %>%
  summarize(Observations = n(),
            "Exit Velocity" = mean(launch_speed),
            "Launch Angle" = mean(launch_angle),
            Distance = mean(hit_distance_sc, na.rm = T)) %>%
  arrange(bb_type)

# homers
flight %>%
  filter(game_year == 2019, events == "home_run") %>%
  group_by(postseason) %>%
  summarize(n = n(),
            avg_velo = mean(launch_speed),
            avg_launch = mean(launch_angle),
            hit_distance_sc = mean(hit_distance_sc, na.rm = T))

# 2019 reg vs post
post_2019 <- cleaned %>%
  filter(game_year == 2019, postseason == 1) %>%
  select(-game_year,-postseason)

set.seed(1234)
rand_order <- sample(1:nrow(reg_2019))
split <- floor(.8*nrow(reg_2019))
index_train <- rand_order < split
index_test <- rand_order >= split
train <- reg_2019[index_train,]
test <- reg_2019[index_test,]

set.seed(1234)
fit_reg_2019 <- randomForest(hit_distance_sc ~ ., data = train,
                           ntree = 10,
                           importance = TRUE)

pred <- predict(fit_reg_2019, test)
y <- test$hit_distance_sc
1 - sum((y-pred)^2)/sum((y-mean(y))^2)

varImpPlot(fit_reg_2019, type = 1)


# postseason
pred_post <- predict(fit_reg_2019, post_2019)
y <- post_2019$hit_distance_sc
1 - sum((y-pred_post)^2)/sum((y-mean(y))^2)

diff_2019 <- post_2019$hit_distance_sc - pred_post
plot_diff(diff_2019)


# combine year and post
flight %>%
  group_by(game_year, postseason) %>%
  summarize(n = n(),
            avg_velo = mean(launch_speed),
            avg_launch = mean(launch_angle),
            hit_distance_sc = mean(hit_distance_sc, na.rm = T)) %>%
  arrange(game_year)

# homers
flight %>%
  filter(events == "home_run") %>%
  group_by(game_year, postseason) %>%
  summarize(n = n(),
            avg_velo = mean(launch_speed),
            avg_launch = mean(launch_angle),
            hit_distance_sc = mean(hit_distance_sc, na.rm = T)) %>%
  arrange(game_year)

  
# 2019 post vs all post
post_2015_2018 <- cleaned %>%
  filter(game_year < 2019, postseason == 1) %>%
  select(-game_year,-postseason)

set.seed(1234)
rand_order <- sample(1:nrow(post_2015_2018))
split <- floor(.8*nrow(post_2015_2018))
index_train <- rand_order < split
index_test <- rand_order >= split
train <- post_2015_2018[index_train,]
test <- post_2015_2018[index_test,]


set.seed(1234)
fit_post_2015_2018 <- randomForest(hit_distance_sc ~ ., data = train,
                           ntree = 10,
                           importance = TRUE)

pred <- predict(fit_post_2015_2018, test)
y <- test$hit_distance_sc
1 - sum((y-pred)^2)/sum((y-mean(y))^2)

varImpPlot(fit_post_2015_2018, type = 1)


# post 2019
pred_post_2019 <- predict(fit_post_2015_2018, post_2019)
y <- post_2019$hit_distance_sc
1 - sum((y-pred_post_2019)^2)/sum((y-mean(y))^2)

diff_post <- post_2019$hit_distance_sc - pred_post_2019
plot_diff(diff_post)


# others seasons reg vs post

# 2018
reg_2018 <- cleaned %>%
  filter(game_year == 2018, postseason == 0) %>%
  select(-game_year,-postseason)

post_2018 <- cleaned %>%
  filter(game_year == 2018, postseason == 1) %>%
  select(-game_year,-postseason)

set.seed(1234)
rand_order <- sample(1:nrow(reg_2018))
split <- floor(.8*nrow(reg_2018))
index_train <- rand_order < split
index_test <- rand_order >= split
train <- reg_2018[index_train,]
test <- reg_2018[index_test,]

set.seed(1234)
fit_reg_2018 <- randomForest(hit_distance_sc ~ ., data = train,
                             ntree = 10,
                             importance = TRUE)

pred <- predict(fit_reg_2018, test)
y <- test$hit_distance_sc
1 - sum((y-pred)^2)/sum((y-mean(y))^2)

pred_post <- predict(fit_reg_2018, post_2018)
y <- post_2018$hit_distance_sc
1 - sum((y-pred_post)^2)/sum((y-mean(y))^2)

diff_2018 <- post_2018$hit_distance_sc - pred_post

# 2017
reg_2017 <- cleaned %>%
  filter(game_year == 2017, postseason == 0) %>%
  select(-game_year,-postseason)

post_2017 <- cleaned %>%
  filter(game_year == 2017, postseason == 1) %>%
  select(-game_year,-postseason)

set.seed(1234)
rand_order <- sample(1:nrow(reg_2017))
split <- floor(.8*nrow(reg_2017))
index_train <- rand_order < split
index_test <- rand_order >= split
train <- reg_2017[index_train,]
test <- reg_2017[index_test,]

set.seed(1234)
fit_reg_2017 <- randomForest(hit_distance_sc ~ ., data = train,
                             ntree = 10,
                             importance = TRUE)

pred <- predict(fit_reg_2017, test)
y <- test$hit_distance_sc
1 - sum((y-pred)^2)/sum((y-mean(y))^2)

pred_post <- predict(fit_reg_2017, post_2017)
y <- post_2017$hit_distance_sc
1 - sum((y-pred_post)^2)/sum((y-mean(y))^2)

diff_2017 <- post_2017$hit_distance_sc - pred_post

# 2016
reg_2016 <- cleaned %>%
  filter(game_year == 2016, postseason == 0) %>%
  select(-game_year,-postseason)

post_2016 <- cleaned %>%
  filter(game_year == 2016, postseason == 1) %>%
  select(-game_year,-postseason)

set.seed(1234)
rand_order <- sample(1:nrow(reg_2016))
split <- floor(.8*nrow(reg_2016))
index_train <- rand_order < split
index_test <- rand_order >= split
train <- reg_2016[index_train,]
test <- reg_2016[index_test,]

set.seed(1234)
fit_reg_2016 <- randomForest(hit_distance_sc ~ ., data = train,
                             ntree = 10,
                             importance = TRUE)

pred <- predict(fit_reg_2016, test)
y <- test$hit_distance_sc
1 - sum((y-pred)^2)/sum((y-mean(y))^2)

pred_post <- predict(fit_reg_2016, post_2016)
y <- post_2016$hit_distance_sc
1 - sum((y-pred_post)^2)/sum((y-mean(y))^2)

diff_2016 <- post_2016$hit_distance_sc - pred_post

# 2015
reg_2015 <- cleaned %>%
  filter(game_year == 2015, postseason == 0) %>%
  select(-game_year,-postseason)

post_2015 <- cleaned %>%
  filter(game_year == 2015, postseason == 1) %>%
  select(-game_year,-postseason)

set.seed(1234)
rand_order <- sample(1:nrow(reg_2015))
split <- floor(.8*nrow(reg_2015))
index_train <- rand_order < split
index_test <- rand_order >= split
train <- reg_2015[index_train,]
test <- reg_2015[index_test,]

set.seed(1234)
fit_reg_2015 <- randomForest(hit_distance_sc ~ ., data = train,
                             ntree = 10,
                             importance = TRUE)

pred <- predict(fit_reg_2015, test)
y <- test$hit_distance_sc
1 - sum((y-pred)^2)/sum((y-mean(y))^2)

pred_post <- predict(fit_reg_2015, post_2015)
y <- post_2015$hit_distance_sc
1 - sum((y-pred_post)^2)/sum((y-mean(y))^2)

diff_2015 <- post_2015$hit_distance_sc - pred_post

# plot
plot_2018 <- plot_diff(diff_2018) +
  ggtitle("2018")
plot_2017 <- plot_diff(diff_2017) +
  ggtitle("2017")
plot_2016 <- plot_diff(diff_2016) +
  ggtitle("2016")
plot_2015 <- plot_diff(diff_2015) +
  ggtitle("2015")


grid.arrange(plot_2018, plot_2017, plot_2016, plot_2015, nrow = 2)

