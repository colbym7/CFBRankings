library(tidyverse)
library(BradleyTerry2)
library(vroom)
install.packages("https://cran.r-project.org/src/contrib/Archive/cfbfastR/cfbfastR_1.6.4.tar.gz", repos = NULL, type = "source")
remotes::install_github("cfbfastR/cfbfastR")

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

bt2_football_mod <- update(bt1_football_mod, formula = ~team + total_home_EPA)

summary(bt2_football_mod)
abilities2 <- BTabilities(bt2_football_mod)



### Compare Performance of BT Model with Moneyline 2023 ###
# Data
cfb_data2023 <- load_cfb_pbp(2023)
bet_data2023 <- vroom('bet_data.csv')
