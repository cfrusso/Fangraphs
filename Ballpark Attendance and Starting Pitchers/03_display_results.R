setwd("~/Attendence By Pitcher")

load("results.rda")

library(dplyr)
library(ggplot2)
library(reshape2)

results$year <- as.numeric(results$year)
results <- results %>%
  filter(year > 1937)
results.long <- melt(results, id = "year",
                     measure = c("Best", "Above_avg", "Below_avg", "Worst"))


ggplot(results.long, aes(x = year, y = value)) +
  geom_point(col = "blue") +
  facet_wrap(~variable) +
  geom_hline(yintercept = 0, color = "darkorange") +
  scale_x_continuous(breaks = seq(1935, 2019, 10)) +
  geom_smooth(col = "cornflowerblue") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(size = 15)) +
  ylab("differential")


by(results.long$value, results.long$variable, mean)


results <- results %>%
  mutate(decade = ifelse(year < 1950, "1938-1949",
                   ifelse(year < 1960, "1950-1959",
                    ifelse(year < 1970, "1960-1969",
                     ifelse(year < 1980, "1970-1979",
                      ifelse(year < 1990, "1980-1989",
                       ifelse(year < 2000, "1990-1999",
                        ifelse(year < 2010, "2000-2009",
                         "2010-2018"))))))))

results %>%
  mutate(tot = Best_num + Above_avg_num +
                    Below_avg_num + Worst_num) %>%
  group_by(decade) %>%
  summarize(Best = mean(Best),
            Above_avg = mean(Above_avg),
            Below_avg = mean(Below_avg),
            Worst = mean(Worst),
            Rsquared = mean(model_Rsquared),
            Pitchers = mean(tot))




