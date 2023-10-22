library(tidyverse)
library(nbastatR)
library(nbaTools)
library(ggimage)
library(gt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(png)
library(paletteer)
library(rvest)
library(purrr)
install.packages("hoopR")
library(hoopR)

Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)
bref_players_stats(seasons = c(1980:2023), tables = "totals", widen = TRUE, assign_to_environment = TRUE)

basic_stats <- dataBREFPlayerTotals %>%
  mutate(player = namePlayer, season = yearSeason + 1) %>%
  filter(countGames >= 41) %>% 
  group_by(slugSeason, season, player) %>%
  summarize(id = slugPlayerBREF, ppg = ptsTotals/countGames, apg = astTotals/countGames, rpg = trbTotals/countGames, spg = stlTotals/countGames, bpg = blkTotals/countGames)

bref_players_stats(seasons = c(1980:2023), tables = "advanced", widen = TRUE, assign_to_environment = TRUE)

advanced_stats <- dataBREFPlayerAdvanced %>%
  mutate(player = namePlayer, season = yearSeason + 1) %>%
  filter(countGames >= 41) %>% 
  group_by(season, player) %>%
  summarize(team = slugTeamsBREF, position = slugPosition, per = ratioPER, ws48 = ratioWSPer48, bpm = ratioBPM, vorp = ratioVORP)

pbp <- game_logs(seasons = c(1980:2023))

wins <- pbp %>%
  mutate(outcome = ifelse(outcomeGame == "W", 1, 0)) %>%
  group_by(yearSeason, namePlayer) %>%
  filter(n() >= 41) %>% 
  summarize(win = sum(outcome))

basic_stats <- inner_join(basic_stats, wins, by = c("season"="yearSeason", "player"="namePlayer"))

stats <- inner_join(basic_stats, advanced_stats, by = c("season", "player"))

seasons <- stats %>%
  select(slugSeason, season, player)

mvp_awards <- bref_awards(awards = "Most Valuable Player")

mvp_awards[mvp_awards == "Nikola Jokić"] <- "Nikola Jokic"

seasons <- left_join(seasons, mvp_awards, by = c("slugSeason", "player"="namePlayer"))

seasons <- seasons %>%
  mutate(mvp = ifelse(is.na(slugAward), 0, 1)) %>%
  select(season, player, mvp)

stats <- inner_join(stats, seasons, by = c("season", "player"))

stats_reg <- glm(mvp ~ ppg + apg + rpg + spg + bpg + win + per + ws48 + bpm + vorp, data = stats, family = binomial)

p_pbp <- game_logs(seasons = c(1980:2023), season_types = "Playoffs")

p_wins <- p_pbp %>%
  mutate(outcome = ifelse(outcomeGame == "W", 1, 0)) %>%
  group_by(yearSeason, namePlayer) %>%
  summarize(p_win = sum(outcome))


stats <- stats %>%
  ungroup() %>%
  mutate(prediction = predict(stats_reg, stats, type = "response")) %>%
  group_by(season) %>%
  mutate(mvp_prob = prediction/sum(prediction)) %>%
  ungroup() %>%
  mutate(li = substring(id, 0, 1)) %>%
  mutate(url = paste0("https://www.basketball-reference.com/players/",li,"/",id,".html"))

test_url <- "https://www.basketball-reference.com/players/h/hieldbu01.html"
playoff_total_stats_test <- read_html(test_url) %>% html_element("#playoffs_per_game") %>% html_table()
playoff_advanced_stats_test <- read_html(test_url) %>% html_element("#playoffs_advanced") %>% html_table() 


og_stats <- read_html(url) %>% html_element("#playoffs_per_game") %>% html_table()
if (length(og_stats) == 0 || all(sapply(og_stats, function(x) class(x) == "xml_missing"))) {
  og_stats = data.frame()
  print(u)
} else {
  og_stats <- og_stats %>%
    mutate(url = url) %>%
    select(season = Season, url, p_ppg = PTS, p_apg = AST, p_rpg = TRB, p_spg = STL, p_bpg = BLK)
  career_index <- which(og_stats$season == "Career")[1]
  og_stats <- og_stats %>% filter(row_number() < career_index)
  return(as.data.frame(og_stats))
}
counter <- 0
get_playoffs_totals <- function(url) {
  og_stats <- read_html(url) %>% html_element("#playoffs_per_game") %>% html_table()
  if (length(og_stats) == 0 || all(sapply(og_stats, function(x) class(x) == "xml_missing"))) {
    og_stats = data.frame()
    print(u)
  } else {
    og_stats <- og_stats %>%
      mutate(url = url) %>%
      select(season = Season, url, p_ppg = PTS, p_apg = AST, p_rpg = TRB, p_spg = STL, p_bpg = BLK)
    career_index <- which(og_stats$season == "Career")[1]
    og_stats <- og_stats %>% filter(row_number() < career_index)
    return(as.data.frame(og_stats))
  }
}
get_playoffs_advanced <- function(url) {
  og_stats <- read_html(url) %>% html_element("#playoffs_advanced") %>% html_table() 
  if (length(og_stats) == 0 || all(sapply(og_stats, function(x) class(x) == "xml_missing"))) {
    og_stats = data.frame()
  } else {
    og_stats <- og_stats[-c(20, 25)] %>%
      mutate(url = url) %>%
      select(season = Season, url, p_per = PER, p_ws48 = "WS/48", p_bpm = BPM, p_vorp = VORP)
    career_index <- which(og_stats$season == "Career")[1]
    og_stats <- og_stats %>% filter(row_number() < career_index)
    return(as.data.frame(og_stats))
  }
}

get_playoffs_combined <- function(url) {
  return(inner_join(get_playoffs_totals(url), get_playoffs_advanced(url), by = c("season", "url")))
}

test_1 <- get_playoffs_totals("https://www.basketball-reference.com/players/o/onealsh01.html")
test_2 <- get_playoffs_advanced("https://www.basketball-reference.com/players/o/onealsh01.html")
combined_test <- get_playoffs_combined("https://www.basketball-reference.com/players/o/onealsh01.html")

distinct_urls <- unique(stats$url)

distinct_stats <- stats %>% distinct(url)
row_num <- which(distinct_stats$url == "https://www.basketball-reference.com/players/p/paytoel01.html")

playoff_stats <- data.frame(matrix(ncol = 11, nrow = 0))
playoff_cols <- c("season", "url", "p_ppg", "p_apg", "p_rpg", "p_spg", "p_bpg", "p_per", "p_ws48", "p_bpm", "p_vorp")
colnames(playoff_stats) <- playoff_cols

valid_links <- character(0)
non_count = 0

for (link in distinct_urls) {
  Sys.sleep(10)
    page <- read_html(link)
    selected_nodes <- page %>% html_nodes("#playoffs_per_game, #playoffs_advanced")
    if (length(selected_nodes) > 0) {
       cat("Processing link:", link, "\n")
       combined_playoffs_data <- get_playoffs_combined(link)
       playoff_stats <- rbind(playoff_stats, combined_playoffs_data)
    } else {
      cat("Skipping link:", link, " (xml_missing class found)\n")
    }
}

p_stats_final <- left_join(playoff_stats, stats, by = c("season"="slugSeason.x", "url")) 

p_stats_final <- p_stats_final %>% distinct(season, url, .keep_all = TRUE)
  
p_stats_no_na <- p_stats_final %>% filter(!is.na(player))

playoff_stats_actual <- left_join(p_stats_no_na, p_wins, by = c("season.y" = "yearSeason", "player" = "namePlayer")) %>%
  select(slugSeason = season, season = season.y, player, team, p_ppg, p_apg, p_rpg, p_spg, p_bpg, p_win, p_per, p_ws48, p_bpm, p_vorp)

playoff_stats_all <- left_join(playoff_stats_actual, mvp_awards, by = c("slugSeason", "player"="namePlayer"))

playoff_stats_all <- playoff_stats_all %>%
  mutate(mvp = ifelse(is.na(slugAward), 0, 1)) %>%
  select(season, player, team, p_ppg, p_apg, p_rpg, p_spg, p_bpg, p_win, p_per, p_ws48, p_bpm, p_vorp, mvp)

p_stats_reg <- glm(mvp ~ p_ppg + p_apg + p_rpg + p_spg + p_bpg + p_win + p_per + p_ws48 + p_bpm + p_vorp, data = playoff_stats_all, family = binomial)


playoff_stats_all <- playoff_stats_all %>%
  ungroup() %>%
  mutate(p_prediction = predict(p_stats_reg, playoff_stats_all, type = "response")) %>%
  group_by(season) %>%
  mutate(p_mvp_prob = p_prediction/sum(p_prediction)) %>%
  ungroup()
  
comparison <- left_join(playoff_stats_all, stats, by = c("season", "player")) %>%
  mutate(diff_mvp = p_mvp_prob - mvp_prob, diff_mvp_pct = (p_mvp_prob - mvp_prob)/(mvp_prob) * 100) %>%
  filter(mvp_prob >= 0.01 & p_mvp_prob >= 0.01) %>%
  select(season, player, team = team.x, mvp_prob, p_mvp_prob, diff_mvp, diff_mvp_pct) 

comparison_mean <- comparison %>%
  group_by(player) %>%
  mutate(seasons = n(), meanchange = mean(diff_mvp_pct)) %>%
  distinct(player, seasons, meanchange)  %>%
  ungroup()
  
comparison %>% arrange(diff_mvp_pct) %>% filter(row_number() <= 10) %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, team, season, mvp_prob, p_mvp_prob, diff_mvp, diff_mvp_pct)
  ) %>%
  data_color(
    columns = c(mvp_prob, p_mvp_prob, diff_mvp, diff_mvp_pct),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    season = md("**Season**"),
    mvp_prob = md("**Regular Season MVP Probability**"),
    p_mvp_prob = md("**Playoffs MVP Probability**"),
    diff_mvp = md("**Difference in MVP Probability**"),
    diff_mvp_pct = md("**Percent Change in MVP Probability**")
  ) %>%
  tab_header(
    title = md("**Largest Playoff Droppers**"),
    subtitle = "Based on NBA MVP Data from 1980 - 2023, Filtered So Players Had At Least > 1% Chance in Regular Season and Playoffs, Uses Basic and Advanced Stats"
  )

comparison_mean %>% arrange(meanchange) %>% filter(row_number() <= 10) %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, seasons, meanchange)
  ) %>%
  data_color(
    columns = meanchange,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    seasons = md("**Seasons**"),
    meanchange = md("**Mean Percent Change in MVP Probability**")
  ) %>%
  tab_header(
    title = md("**Largest Average Playoff Droppers**"),
    subtitle = "Based on NBA MVP Data from 1980 - 2023, Filtered So Players Had At Least > 1% Chance in Regular Season and Playoffs, Uses Basic and Advanced Stats"
  )