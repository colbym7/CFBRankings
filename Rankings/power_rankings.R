library(tidyverse)
library(BradleyTerry2)
library(vroom)
remotes::install_github("saiemgilani/cfbfastR", force = TRUE)
library(cfbfastR)


game_results2019_df <- vroom('2019GameResults.csv')


## Bradley Terry Model Exploration ##
data("baseball", package = 'BradleyTerry2')
head(baseball)

baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      data = baseball, id = "team")
summary(baseballModel1)

baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)
summary(baseballModel2)

BTabilities(baseballModel2)



### Simple Bradley Terry with 2019 College Football Data ###
game_results2019_df <- game_results2019_df %>%
  filter(
    HomeClassification == 'fbs' & AwayClassification == 'fbs'
  ) %>%
  mutate(
    HomeWin = ifelse(HomePoints>AwayPoints, 1, 0),
    AwayWin = ifelse(AwayPoints>HomePoints, 1, 0)
  )
head(game_results2019_df)
small2019 <- game_results2019_df %>%
  select(c(2,6,7, 10, 15, 16, 19, 24, 25))


small2019$HomeWin <- as.factor(small2019$HomeWin)
small2019$AwayWin <- as.factor(small2019$AwayWin)

teams <- sort(unique(c(small2019$HomeTeam, small2019$AwayTeam)))
small2019$HomeFactor <- factor(small2019$HomeTeam, levels = teams)
small2019$AwayFactor <- factor(small2019$AwayTeam, levels = teams)

bt1_football_mod <- BTm(cbind(HomeWin, AwayWin), HomeFactor, AwayFactor,
                      data = small2019)
summary(bt1_football_mod)
abilities <- BTabilities(bt1_football_mod)

team_ranking <- as.data.frame(abilities) %>%
  tibble::rownames_to_column("Team") %>%
  arrange(desc(ability)) %>%
  mutate(Rank = row_number(),
         BYUProb = exp(-0.33131175)/(exp(-0.33131175)+exp(ability)))

team_ranking


  # Bradley Terry Model with Total Expected Points Added #
cfb_data2019 <- load_cfb_pbp(2019)
head(cfb_data2019)

  # Filter for Total EPA only last play #
unique_phrase <- unique(cfb_data2019$play_type)
endgame2019 <- cfb_data2019 %>%
  group_by(game_id) %>%
  filter(game_play_number == max(game_play_number) & play_type == 'End Period') %>%
  ungroup()


  # Merge pbp and game results data
small2019 <- small2019 %>%
  mutate(GameKey = paste(Week, pmin(HomeTeam, AwayTeam), pmax(HomeTeam, AwayTeam), sep = "_"))

endgame2019 <- endgame2019 %>%
  mutate(GameKey = paste(week, pmin(pos_team, def_pos_team), pmax(pos_team, def_pos_team), sep = "_"))

expected_dif_df <- small2019 %>%
  left_join(endgame2019, by = "GameKey")

  # Original & more complex BT mods
bt1_football_mod <- BTm(cbind(HomeWin, AwayWin), HomeFactor, AwayFactor,
                        data = expected_dif_df)

bt2_football_mod <- update(bt1_football_mod, formula = ~ team + total_home_EPA, id = 'team')

summary(bt2_football_mod)
abilities2 <- BTabilities(bt2_football_mod)



### Compare Performance of BT Model with Moneyline 2023 ###
# Data
cfb_data2024 <- load_cfb_pbp(2024)
bet2024 <- cfbd_betting_lines(year = 2024)
game_results2024 <- cfbd_game_info(2024)

# Prepare Data #
game_results2024 <- game_results2024 %>%
  filter(home_division == 'fbs' & away_division == 'fbs') %>%
  select(-c(6,7,11,12,19,20,27:31))

bet2024 <- bet2024 %>%
  filter(home_classification == 'fbs' & away_classification == 'fbs')
# Find out which betting provider has the least missing data
bet2024 %>%
  group_by(provider) %>%
  summarise(
    n_missing_home_moneyline = sum(is.na(home_moneyline)),
    n_missing_away = sum(is.na(away_moneyline)),
    n_total = n()
  ) %>%
  mutate(
    pct_missing = n_missing_home_moneyline / n_total * 100
  )

bet2024 <- bet2024 %>%
  filter(provider == 'DraftKings')

cfb_full2024 <- game_results2024 %>%
  left_join(
    bet2024 %>%
      select(game_id, everything()) %>%                 # ensure game_id stays
      select(-setdiff(intersect(names(game_results2024), names(.)), "game_id")),  
    by = "game_id"
  )

# Check NA's
nas <- cfb_full2024 %>%
  filter(is.na(home_points) | is.na(away_points) | is.na(home_moneyline))

cfb_full2024 %>%
  summarize(
    home_favored = sum(home_moneyline < away_moneyline, na.rm = TRUE),
    equal        = sum(home_moneyline == away_moneyline, na.rm = TRUE),
    away_favored = sum(away_moneyline < home_moneyline, na.rm = TRUE)
  )
equal_strength <- cfb_full2024 %>%
  filter(home_moneyline == away_moneyline)


cfb_full2024 <- cfb_full2024 %>%
  dplyr::select(-tidyselect::ends_with(".y")) %>%
  mutate(
    vegas_home_win = case_when(
      home_moneyline < away_moneyline ~ 1,   # home favored
      home_moneyline >= away_moneyline ~ 0,  # away favored
      TRUE ~ NA_real_)
  ) %>%
  mutate(
    HomeWin = ifelse(home_points>away_points, 1, 0),
    AwayWin = ifelse(away_points>home_points, 1, 0)
  ) %>%
  mutate(
    vegas_correct = case_when(
      vegas_home_win == HomeWin ~ 1,
      vegas_home_win != HomeWin ~ 0,
      TRUE ~ NA_real_
    )
  )



cfb_full2024$HomeWin <- as.factor(cfb_full2024$HomeWin)
cfb_full2024$AwayWin <- as.factor(cfb_full2024$AwayWin)

teams <- sort(unique(c(cfb_full2024$home_team, cfb_full2024$away_team)))
cfb_full2024$HomeFactor <- factor(cfb_full2024$home_team, levels = teams)
cfb_full2024$AwayFactor <- factor(cfb_full2024$away_team, levels = teams)
  

vegasVsBt <- function(week_num){
  temp_currentweekdata <- cfb_full2024 %>%
    filter(week == week_num)
  temp_btdata <- cfb_full2024 %>% # Filter complete dataset to week previous for btmod
    filter(week < week_num) %>%
    filter(!is.na(HomeWin), !is.na(AwayWin), !is.na(HomeFactor), !is.na(AwayFactor))
    
  temp_vegas_data <- cfb_full2024 %>% # Filter dataset for getting betting percentages current week
    filter(week == week_num)
  temp_vegas_correct <- mean(temp_vegas_data$vegas_correct, na.rm = TRUE) # Betting % Correct

  temp_btmod <- BTm(cbind(HomeWin, AwayWin), HomeFactor, AwayFactor,
                                       data = temp_btdata) # BT on previous week
  temp_abilities <- BTabilities(temp_btmod)
  
  temp_abilities_df <- as.data.frame(temp_abilities) %>%
    tibble::rownames_to_column("Team")
  
  temp_currentweekdata <- temp_currentweekdata %>%
    left_join(temp_abilities_df %>% rename(home_team = Team, home_ability = ability),
              by = "home_team") %>%
    left_join(temp_abilities_df %>% rename(away_team = Team, away_ability = ability),
              by = "away_team")
  temp_currentweekdata <- temp_currentweekdata %>%
    mutate(
      Model_homewin = ifelse(
        home_ability >= away_ability, 1, 0
      )
    ) %>%
    mutate(
      model_correct = case_when(
        Model_homewin == HomeWin ~ 1,
        Model_homewin != HomeWin ~ 0,
        TRUE ~ NA_real_
      ))
  model_percent <- mean(temp_currentweekdata$model_correct, na.rm = TRUE)
  return(paste0('Model Proportion Correct:',round(model_percent,3),' ',  
                'Vegas Proportion Correct:',round(temp_vegas_correct,3),' ', 
                ' Model Difference: ',round(model_percent-temp_vegas_correct, 3)))
}


vegasVsBt(8)
vegasVsBt(9)
vegasVsBt(10)
vegasVsBt(11)
vegasVsBt(12)
vegasVsBt(13)
vegasVsBt(14)
vegasVsBt(15)

