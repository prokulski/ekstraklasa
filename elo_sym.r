## Skrypt symuluje historycznie rozegrane mecze i ich wyniki zgodnie z rankingiem ELO

setwd("~/RProjects/ekstraklasa")

library(tidyverse)
library(lubridate)

rm(list=ls())

# funkcje do liczenia wyniku meczy
source("elo_functions.r")

# te parametry trzeba znaleźć
elo_draw <- 0 # szansa na remis; jesli =0 to będzie użyty rnorm(1, 97.3, 2)
elo_home <- 0 # przewada gospodarzy; jesli =0 to będzie użyty rnorm(1, 61, 1)

mecze <- readRDS("mecze.RDS")
mecze <- mecze %>% mutate(n = row_number())

################################
#### SYMULACJA - PARAMETRY  ####
################################

deb_prn <- FALSE

# początkowe ELO - to co było dla drużyn na clubelo.com na 2008-08-08
# a dla reszty - mediana (1254)
elo_tab <- data_frame(team = c("Wisła Kraków", "Legia Warszawa", "Lech Poznań", "Cracovia", "GKS Bełchatów", "Ruch Chorzów", "ŁKS Łódź", "Górnik Zabrze", "Polonia Bytom", "Odra Wodzisław Śl.", "Arka Gdynia", "Polonia Warszawa", "Piast Gliwice", "Lechia Gdańsk", "Śląsk Wrocław", "Jagiellonia Białystok", "Korona Kielce", "Zagłębie Lubin", "Widzew Łódź", "Podbeskidzie Bielsko-Biała", "Pogoń Szczecin", "Zawisza Bydgoszcz", "Górnik Łęczna", "Termalica Bruk-Bet Nieciecza", "Wisła Płock"),
                      elo = c(1565, 1485, 1426, 1333, 1326, 1288, 1285, 1257, 1252, 1250, 1229, 1229, 1229, 1229, 1229, 1211, 1254, 1254, 1254, 1254, 1254, 1254, 1254, 1254, 1254))

# tabela na historię ELO
elo_tab_hist <- data_frame()

# tabela z meczami i prawdziwymi wynikami
mecze_sym <- mecze[, 3:6]

# miejsce na wyniki symulacji - bramki
mecze_sym$b_gosp_sym <- NA
mecze_sym$b_gosc_sym <- NA
################################


################################
####  SYMULACJA HISTORII    ####
################################

# symulacja - każdy kolejny mecz w lidze
for(n_mecz in 1:nrow(mecze_sym)) {

  # wybór grających drużyn
  team_a <- as.character(mecze_sym[n_mecz, "gosp"])
  team_b <- as.character(mecze_sym[n_mecz, "gosc"])

  # aktualne ELO grających drużyn
  elo_a <- as.integer(elo_tab[elo_tab$team == team_a, "elo"])
  elo_b <- as.integer(elo_tab[elo_tab$team == team_b, "elo"])

  # rozegraj wirtualny mecz i weź wyniki
  tmp_score <- playMatch(team_a, team_b, elo_a, elo_b, 1000, elo_draw, elo_home)
  score_a <- tmp_score[1]
  score_b <- tmp_score[2]

  # wylicz nowe ELO
  tmp_elo <- calcNewElo(score_a, score_b, elo_a, elo_b)

  # podsumowanie meczu
  if(deb_prn)
  {
    cat(paste0("Wynik meczu: ", team_a, " - ", team_b, " ", score_a, ":", score_b, "\n",
               "Zmiana ELO:\n\t", team_a, ": ", elo_a, " -> ", tmp_elo[1],
               "\n\t", team_b, ": ", elo_b, " -> ", tmp_elo[2]))
  }

  # zmiana ELO w tabeli
  elo_tab[elo_tab$team == team_a, "elo"] <- tmp_elo[1]
  elo_tab[elo_tab$team == team_b, "elo"] <- tmp_elo[2]

  # zapisanie historii ELO
  elo_tab_hist <- bind_rows(elo_tab_hist,
                            data_frame(n=rep(n_mecz, 2),
                                       team = c(team_a, team_b),
                                       elo = c(tmp_elo[1], tmp_elo[2])))

  # zapisanie wyników symulowanych - bramki
  mecze_sym[n_mecz, "b_gosp_sym"] <- score_a
  mecze_sym[n_mecz, "b_gosc_sym"] <- score_b

  if(deb_prn) cat("\n=====================\n\n")
}
################################


################################
#### PODSUMOWANIE SYMULACJI ####
################################

# podsumowanie symulacji
# kto wygrał w rzeczywistości, a kto w symulacji?
mecze_sym <- mecze_sym %>%
  mutate(wygrany_real = ifelse(b_gosp>b_gosc, "gosp",
                               ifelse(b_gosp==b_gosc, "remis", "gosc")),
         wygrany_sym = ifelse(b_gosp_sym>b_gosc_sym, "gosp",
                              ifelse(b_gosp_sym==b_gosc_sym, "remis", "gosc")))


table(mecze_sym$wygrany_real, mecze_sym$wygrany_sym,
      dnn=c("rzeczywistosc", "symulacja"))

# trafność
sum(diag(table(mecze_sym$wygrany_real, mecze_sym$wygrany_sym)))/nrow(mecze_sym)

# przebieg historii ELO
elo_tab_hist %>%
  ggplot() +
  geom_line(aes(n, elo, color=team)) +
  facet_wrap(~team)
################################


