library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
options(scipen = 9999)
data <- load_pbp(2023)
pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa), !is.na(cp), !is.na(cpoe))
pbp_rp %>%
  filter(pass == 1) %>%
  group_by(passer) %>%
  summarize(
    mean_epa = mean(qb_epa), tds = sum(pass_touchdown) , 
    cp = mean(cp), cpoe = mean(cpoe), plays = n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 10)
qbs <- pbp_rp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 10 & n_plays > 10)
load_teams()
qbs <- qbs %>%
  left_join(load_teams(), by = c('team' = 'team_abbr'))
qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos (this uses nflplotR package)
  geom_nfl_logos(aes(team_abbr = team), width = qbs$n_plays / 1100, alpha = 0.75) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2023",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

