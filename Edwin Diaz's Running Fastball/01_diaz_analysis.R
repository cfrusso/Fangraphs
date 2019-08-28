setwd("~/Fangraphs Blog/07_Diaz")

diaz_char <- read.csv("diaz.csv", stringsAsFactors = F)

library(dplyr)
library(ggplot2)

diaz <- diaz_char %>%
  mutate(release_speed = as.numeric(release_speed),
         release_spin_rate = as.numeric(release_spin_rate),
         release_extension = as.numeric(release_extension),
         release_pos_x = as.numeric(release_pos_x),
         release_pos_z = as.numeric(release_pos_z),
         estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle),
         plate_x = as.numeric(plate_x),
         plate_z = as.numeric(plate_z),
         pfx_x = as.numeric(pfx_x),
         pfx_z = as.numeric(pfx_z))

# summary
diaz %>%
  filter(pitch_type == "FF") %>%
  group_by(game_year) %>%
  summarize(EXT = mean(release_extension, na.rm = T),
            velo = mean(release_speed, na.rm = T),
            spin = mean(release_spin_rate, na.rm = T),
            rel_x = mean(release_pos_x, na.rm = T),
            rel_z = mean(release_pos_z, na.rm = T),
            pfx_x = mean(pfx_x, na.rm = T),
            pfx_z = mean(pfx_z, na.rm = T)) %>%
  arrange(game_year)

# filter 4-seamers
fbs <- diaz %>%
  filter(pitch_type == "FF")


# Release Extension

ggplot(fbs, aes(x = release_extension, color = factor(game_year))) +
  geom_density() +
  theme_minimal() +
  theme(panel.background = element_rect(color = "black", size=2)) +
  xlab("Release Extension") +
  ylab("Density") +
  labs(color = "Year")
  
fbs$ext_type <- ifelse(fbs$release_extension >= 6.7, "long", "normal")

fbs %>%
  group_by(ext_type, game_year) %>%
  summarize(pitches = n(),
            EXT = mean(release_extension, na.rm = T),
            velo = mean(release_speed, na.rm = T),
            spin = mean(release_spin_rate, na.rm = T),
            rel_x = mean(release_pos_x, na.rm = T),
            rel_z = mean(release_pos_z, na.rm = T),
            pfx_x = mean(pfx_x, na.rm = T),
            pfx_z = mean(pfx_z, na.rm = T),
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = T)) %>%
  arrange(game_year)

ggplot(fbs, aes(plate_x, plate_z, color = ext_type)) +
  geom_point() +
  facet_wrap(~game_year) +
  geom_rect(aes(xmin = -1, xmax = 1,
                ymin = 1.5,ymax = 3.5),
            color = "red", alpha = 0) +
  theme_minimal() +
  labs(color = "Type") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Pitch Distribution")


# Horizontal Release Position

ggplot(fbs, aes(x = release_pos_x, color = factor(game_year))) +
  geom_density() +
  theme_minimal() +
  theme(panel.background = element_rect(color = "black", size=2)) +
  xlab("Horizontal Release Position") +
  ylab("Density") +
  labs(color = "Year")

fbs$x_type <- ifelse(fbs$release_pos_x <= -2.875, "long", "normal")

fbs[fbs$game_year > 2017,] %>%
  group_by(x_type, game_year) %>%
  summarize(pitches = n(),
            EXT = mean(release_extension, na.rm = T),
            velo = mean(release_speed, na.rm = T),
            spin = mean(release_spin_rate, na.rm = T),
            rel_x = mean(release_pos_x, na.rm = T),
            rel_z = mean(release_pos_z, na.rm = T),
            pfx_x = mean(pfx_x, na.rm = T),
            pfx_z = mean(pfx_z, na.rm = T),
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = T)) %>%
  arrange(game_year)

ggplot(fbs[fbs$game_year > 2017,], aes(plate_x, plate_z, color = x_type)) +
  geom_point() +
  facet_wrap(~game_year) +
  geom_rect(aes(xmin = -1, xmax = 1,
                ymin = 1.5,ymax = 3.5),
            color = "red", alpha = 0) +
  theme_minimal() +
  labs(color = "Type") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Pitch Distribution")


# Vertical Release Position

ggplot(fbs, aes(x = release_pos_z, color = factor(game_year))) +
  geom_density() +
  theme_minimal() +
  theme(panel.background = element_rect(color = "black", size=2)) +
  xlab("Vertical Release Position") +
  ylab("Density") +
  labs(color = "Year")

fbs$z_type <- ifelse(fbs$release_pos_z <= 5.1,"long", "normal")

fbs[fbs$game_year > 2017,] %>%
  group_by(z_type, game_year) %>%
  summarize(pitches = n(),
            EXT = mean(release_extension, na.rm = T),
            velo = mean(release_speed, na.rm = T),
            spin = mean(release_spin_rate, na.rm = T),
            rel_x = mean(release_pos_x, na.rm = T),
            rel_z = mean(release_pos_z, na.rm = T),
            pfx_x = mean(pfx_x, na.rm = T),
            pfx_z = mean(pfx_z, na.rm = T),
            xwOBA = mean(estimated_woba_using_speedangle, na.rm = T)) %>%
  arrange(game_year)

ggplot(fbs[fbs$game_year > 2017,], aes(plate_x, plate_z, color = z_type)) +
  geom_point() +
  facet_wrap(~game_year) +
  geom_rect(aes(xmin = -1, xmax = 1,
                ymin = 1.5,ymax = 3.5),
            color = "red", alpha = 0) +
  theme_minimal() +
  labs(color = "Type") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Pitch Distribution")
