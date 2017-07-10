## Skrypt znajduje najlepsze parametry dla funkcji liczących
## wynik wirtualnego meczu
##
## Parametry te to:
## elo_d - szansa na remis
## elo_h - przewaga gospodarzy
##
## Skrypt dla każdej kombinacji zadanych par parametów testowych przebiega całą
## historię meczy, rozgrywa je wirtualnie i liczy trafność wyników
## (wygrany/remis - bez wnikania w wynik bramkowy)
##
## Za najlepszą parę elo_d i elo_h uważamy tę, która dała najwyższą trafność
##


# podejscie w zakresach
# elo_d = seq(50, 150, 10)
# elo_h = seq(50, 120, 10)
# n_prob = 5
#
#   elo_d elo_h accuracy
# 1   120   120 45.17617
# 2    90   120 44.29530
# 3    60   120 44.12752
# 4    90   110 43.83389
# 5   110   120 43.83389
# 6    50   120 43.79195


# podejscie w zakresach
# elo_d = seq(50, 300, 25)
# elo_h = seq(100, 200, 25)
# n_prob = 1
#   elo_d elo_h accuracy
# 1   125   175 47.39933
# 2    75   200 47.14765
# 3    50   175 47.10570
# 4   100   150 47.06376
# 5   100   200 47.02181
# 6    75   150 46.97987


# podejscie w zakresach
# elo_d = seq(0, 300, 5)
# elo_h = seq(0, 300, 5)
# n_prob = 1000



setwd("~/RProjects/ekstraklasa")

library(tidyverse)
library(lubridate)

rm(list=ls())

# funkcje do liczenia wyniku meczy
source("elo_functions.r")



mecze <- readRDS("mecze.RDS")
mecze <- mecze %>% mutate(n = row_number())

################################
####  FIND GOOD PARAMS      ####
################################

elo_params <- expand.grid(elo_d = seq(0, 300, 5),
                          elo_h = seq(0, 300, 5))
elo_params$accuracy <- NA

n_prob <- 1000

l_params <- nrow(elo_params)

# dla każdej kombinacji parametrów
for(k in 1:l_params) {
  # zerujemy ELO
  elo_tab <- data_frame(team = c("Wisła Kraków", "Legia Warszawa", "Lech Poznań", "Cracovia", "GKS Bełchatów", "Ruch Chorzów", "ŁKS Łódź", "Górnik Zabrze", "Polonia Bytom", "Odra Wodzisław Śl.", "Arka Gdynia", "Polonia Warszawa", "Piast Gliwice", "Lechia Gdańsk", "Śląsk Wrocław", "Jagiellonia Białystok", "Korona Kielce", "Zagłębie Lubin", "Widzew Łódź", "Podbeskidzie Bielsko-Biała", "Pogoń Szczecin", "Zawisza Bydgoszcz", "Górnik Łęczna", "Termalica Bruk-Bet Nieciecza", "Wisła Płock"),
                        elo = c(1565, 1485, 1426, 1333, 1326, 1288, 1285, 1257, 1252, 1250, 1229, 1229, 1229, 1229, 1229, 1211, 1254, 1254, 1254, 1254, 1254, 1254, 1254, 1254, 1254))


  elo_d <- as.numeric(elo_params[k, "elo_d"])
  elo_h <- as.numeric(elo_params[k, "elo_h"])

  correct_predict <- 0
  #   cat("\n")
  cat(paste("\rk =", k))

  # symulacja - każdy kolejny mecz w lidze
  for(n_mecz in 1:nrow(mecze)) {
    #      cat(paste("\rk =", k, "/", l_params, "; n_mecz =", n_mecz, "/ 2384"))


    # wybór grających drużyn
    team_a <- as.character(mecze[n_mecz, "gosp"])
    team_b <- as.character(mecze[n_mecz, "gosc"])

    # aktualne ELO grających drużyn
    elo_a <- as.integer(elo_tab[elo_tab$team == team_a, "elo"])
    elo_b <- as.integer(elo_tab[elo_tab$team == team_b, "elo"])

    # rozegraj wirtualny mecz i weź wyniki
    tmp_score <- playMatch(team_a, team_b, elo_a, elo_b, n_prob, elo_d, elo_h)
    score_a <- tmp_score[1]
    score_b <- tmp_score[2]

    # wylicz nowe ELO
    tmp_elo <- calcNewElo(score_a, score_b, elo_a, elo_b)

    # zmiana ELO w tabeli
    elo_tab[elo_tab$team == team_a, "elo"] <- tmp_elo[1]
    elo_tab[elo_tab$team == team_b, "elo"] <- tmp_elo[2]


    # czy wynik przewidywania trafny (kto wygrał lub remis)?
    if( (score_a > score_b) &
        (as.numeric(mecze[n_mecz, "b_gosp"]) > as.numeric(mecze[n_mecz, "b_gosc"])) ) {
      correct_predict <- correct_predict + 1
    }

    if( (score_b > score_a) &
        (as.numeric(mecze[n_mecz, "b_gosc"]) > as.numeric(mecze[n_mecz, "b_gosp"])) ) {
      correct_predict <- correct_predict + 1
    }

    if( (score_b == score_a) &
        (as.numeric(mecze[n_mecz, "b_gosc"]) == as.numeric(mecze[n_mecz, "b_gosp"])) ) {
      correct_predict <- correct_predict + 1
    }
  }

  elo_params[k, "accuracy"] <- correct_predict
}

elo_params$accuracy <- 100 * elo_params$accuracy / nrow(mecze)
################################

# jakie parametry dały nalepsze wyniki?
elo_params %>% arrange(desc(accuracy)) %>% head()

elo_params %>%
  ggplot() +
  geom_tile(aes(elo_d, elo_h, fill = cut(accuracy, 5)))

saveRDS(elo_params, file="elo_params.rds")
