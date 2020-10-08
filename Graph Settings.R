library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyr)

dev.new(width=11,height=5,noRStudioGD = TRUE)

settings_adjustments <- read_excel("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Settings_Adjustments 2020-10-04.xlsx")

data_wide <- settings_adjustments %>%
  filter(ppr_type == 1) %>%
  arrange(desc(qb_adj)) %>%
  mutate(league_settings = paste(num_qbs, "QB,", num_teams, "teams"))

plot_theme <- 
  theme(legend.position = "none",
        plot.title = element_text(size = 22, hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 15, l = 0)),
        plot.subtitle = element_text(size = 18, hjust = 0.5,
                                     margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.x = element_text(size = 16,
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16))

data_wide$league_settings <- factor(data_wide$league_settings, data_wide$league_settings)

data_long <- data_wide %>%
  pivot_longer(qb_adj:pick_adj, names_to = "position", values_to = "adjustment") %>%
  mutate(position = gsub("_adj", "", position)) %>%
  mutate(position = ifelse(position == "pick", "Pick", toupper(position))) %>%
  select(league_settings:adjustment)

ggplot(data = data_long, aes(x = adjustment, y = league_settings, label = position, color = position)) +
  geom_label(alpha = 0.5) +
  labs(x = "Value relative to the player's value in a 1 QB, 12 team league",
       title = "How valuable are players in different league types?",
       subtitle = "Shown by number of QBs and number of teams") +
  theme_fivethirtyeight() +
  plot_theme +
  scale_color_gdocs()

#PPR Graph
data_wide <- settings_adjustments %>%
  filter(num_qbs == 1, num_teams == 12) %>%
  arrange(desc(te_adj)) %>%
  mutate(league_settings = paste(ppr_type, "PPR"))

data_wide$league_settings <- factor(data_wide$league_settings, data_wide$league_settings)

data_long <- data_wide %>%
  pivot_longer(qb_adj:pick_adj, names_to = "position", values_to = "adjustment") %>%
  mutate(position = gsub("_adj", "", position)) %>%
  mutate(position = ifelse(position == "pick", "Pick", toupper(position))) %>%
  select(league_settings:adjustment)

ggplot(data = data_long, aes(x = adjustment, y = league_settings, label = position, color = position)) +
  geom_label(alpha = 0.5) +
  labs(x = "Value relative to the player's value in a 1 PPR league",
       title = "How valuable are players in different league types?",
       subtitle = "Shown by PPR type") +
  theme_fivethirtyeight() +
  plot_theme +
  scale_color_gdocs() +
  scale_x_continuous(limits = c(0.8, 1.4), breaks = seq(0.8, 1.4, by = 0.1), labels = c("0.8", "", "1.0", "", "1.2", "", "1.4"))

#Number of QBs Graph
data_wide <- settings_adjustments %>%
  filter(num_teams == 12, ppr_type == 1) %>%
  arrange(desc(te_adj)) %>%
  mutate(league_settings = paste(num_qbs, "QB"))

data_wide$league_settings <- factor(data_wide$league_settings, data_wide$league_settings)

data_long <- data_wide %>%
  pivot_longer(qb_adj:pick_adj, names_to = "position", values_to = "adjustment") %>%
  mutate(position = gsub("_adj", "", position)) %>%
  mutate(position = ifelse(position == "pick", "Pick", toupper(position))) %>%
  select(league_settings:adjustment)

ggplot(data = data_long, aes(x = adjustment, y = league_settings, label = position, color = position)) +
  geom_label(alpha = 0.5) +
  labs(x = "Value relative to the player's value in a 1 QB league",
       title = "How valuable are players in different league types?",
       subtitle = "Shown by number of QBs") +
  theme_fivethirtyeight() +
  plot_theme +
  scale_color_gdocs() +
  scale_x_continuous(limits = c(0.8, 1.4), breaks = seq(0.8, 1.4, by = 0.1), labels = c("0.8", "", "1.0", "", "1.2", "", "1.4"))

#Number of teams Graph
data_wide <- settings_adjustments %>%
  filter(num_qbs == 1, ppr_type == 1) %>%
  arrange(desc(te_adj)) %>%
  mutate(league_settings = paste(num_teams, "Teams"))

data_wide$league_settings <- factor(data_wide$league_settings, data_wide$league_settings)

data_long <- data_wide %>%
  pivot_longer(qb_adj:pick_adj, names_to = "position", values_to = "adjustment") %>%
  mutate(position = gsub("_adj", "", position)) %>%
  mutate(position = ifelse(position == "pick", "Pick", toupper(position))) %>%
  select(league_settings:adjustment)

ggplot(data = data_long, aes(x = adjustment, y = league_settings, label = position, color = position)) +
  geom_label(alpha = 0.5) +
  labs(x = "Value relative to the player's value in a 12 team league",
       title = "How valuable are players in different league types?",
       subtitle = "Shown by number of teams") +
  theme_fivethirtyeight() +
  plot_theme +
  scale_color_gdocs() +
  scale_x_continuous(limits = c(0.8, 1.4), breaks = seq(0.8, 1.4, by = 0.1), labels = c("0.8", "", "1.0", "", "1.2", "", "1.4"))
