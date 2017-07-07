## Skrypt symuluje rozgrywki w sezonie 2017/18 zgodnie z rankingiem ELO
## Na koniec oblicza tabelę po 30. kolejce

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
#### SYMULACJA SEZONU 17-18 ####
################################

sezon_2017_2018 <- read_csv2("sezon_2017_2018.csv")

delta_n_mecz <- n_mecz

# Aktualne ELO z ClubELO.com
elo_tab <- data_frame(team = c("Wisła Kraków", "Legia Warszawa", "Lech Poznań", "Cracovia", "Pogoń Szczecin", "Zagłębie Lubin", "Wisła Płock", "Sandecja Nowy Sącz", "Lechia Gdańsk", "Termalica Bruk-Bet Nieciecza", "Arka Gdynia", "Jagiellonia Białystok", "Korona Kielce", "Piast Gliwice", "Śląsk Wrocław", "Górnik Zabrze"),
                      elo = c(1365, 1549, 1486, 1310, 1346, 1373, 1295, 1265, 1446, 1289, 1265, 1424, 1320, 1357, 1342, 1286))


# miejsce na wyniki symulacji - bramki
sezon_2017_2018$b_gosp_sym <- NA
sezon_2017_2018$b_gosc_sym <- NA

# symulacja - każdy kolejny mecz w lidze
for(n_mecz in 1:nrow(sezon_2017_2018)) {

  # wybór grających drużyn
  team_a <- as.character(sezon_2017_2018[n_mecz, "Gospodarz"])
  team_b <- as.character(sezon_2017_2018[n_mecz, "Gosc"])

  # aktualne ELO grających drużyn
  elo_a <- as.integer(elo_tab[elo_tab$team == team_a, "elo"])
  elo_b <- as.integer(elo_tab[elo_tab$team == team_b, "elo"])

  # rozegraj wirtualny mecz i weź wyniki
  tmp_score <- playMatch(team_a, team_b, elo_a, elo_b, 1000)
  score_a <- tmp_score[1]
  score_b <- tmp_score[2]

  # wylicz nowe ELO
  tmp_elo <- calcNewElo(score_a, score_b, elo_a, elo_b)

  # zmiana ELO w tabeli
  elo_tab[elo_tab$team == team_a, "elo"] <- tmp_elo[1]
  elo_tab[elo_tab$team == team_b, "elo"] <- tmp_elo[2]

  # zapisanie historii ELO
  elo_tab_hist <- bind_rows(elo_tab_hist,
                            data_frame(n=rep(n_mecz+delta_n_mecz, 2),
                                       team = c(team_a, team_b),
                                       elo = c(tmp_elo[1], tmp_elo[2])))

  # zapisanie wyników symulowanych - bramki
  sezon_2017_2018[n_mecz, "b_gosp_sym"] <- score_a
  sezon_2017_2018[n_mecz, "b_gosc_sym"] <- score_b

  # podsumowanie meczu
  if(deb_prn)
  {
    cat(paste0("Wynik meczu: ", team_a, " - ", team_b, " ", score_a, ":", score_b, "\n",
               "Zmiana ELO:\n\t", team_a, ": ", elo_a, " -> ", tmp_elo[1],
               "\n\t", team_b, ": ", elo_b, " -> ", tmp_elo[2]))
    cat("\n=====================\n\n")
  }
}
################################



################################
#### TABELA - SEZON 17-18   ####
################################

tabela_pkt <- data_frame(team = unique(sezon_2017_2018$Gospodarz),
                         m = rep(0, length(unique(sezon_2017_2018$Gospodarz))),
                         pkt = rep(0, length(unique(sezon_2017_2018$Gospodarz))),
                         bwin = rep(0, length(unique(sezon_2017_2018$Gospodarz))),
                         blost = rep(0, length(unique(sezon_2017_2018$Gospodarz))))
tabela_pkt_pos <- data_frame()

# każdy kolejny mecz w lidze
last_date <- as.integer(sezon_2017_2018[1, "Data"])

for(n_mecz in 1:nrow(sezon_2017_2018)) {
  t_a <- as.character(sezon_2017_2018[n_mecz, "Gospodarz"])
  t_b <- as.character(sezon_2017_2018[n_mecz, "Gosc"])
  b_a <- as.integer(sezon_2017_2018[n_mecz, "b_gosp_sym"])
  b_b <- as.integer(sezon_2017_2018[n_mecz, "b_gosc_sym"])

  # liczba meczy
  tabela_pkt[tabela_pkt$team == t_a, "m"] <- as.integer(tabela_pkt[tabela_pkt$team == t_a, "m"]) + 1
  tabela_pkt[tabela_pkt$team == t_b, "m"] <- as.integer(tabela_pkt[tabela_pkt$team == t_b, "m"]) + 1

  # bramki zyskane
  tabela_pkt[tabela_pkt$team == t_a, "bwin"] <- as.integer(tabela_pkt[tabela_pkt$team == t_a, "bwin"]) + b_a
  tabela_pkt[tabela_pkt$team == t_b, "bwin"] <- as.integer(tabela_pkt[tabela_pkt$team == t_b, "bwin"]) + b_b

  # bramki stracone
  tabela_pkt[tabela_pkt$team == t_a, "blost"] <- as.integer(tabela_pkt[tabela_pkt$team == t_a, "blost"]) + b_b
  tabela_pkt[tabela_pkt$team == t_b, "blost"] <- as.integer(tabela_pkt[tabela_pkt$team == t_b, "blost"]) + b_a


  # punkty
  # wygrany A
  if(b_a > b_b) tabela_pkt[tabela_pkt$team == t_a, "pkt"] <- as.integer(tabela_pkt[tabela_pkt$team == t_a, "pkt"]) + 3

  # wygrany B
  if(b_b > b_a) tabela_pkt[tabela_pkt$team == t_b, "pkt"] <- as.integer(tabela_pkt[tabela_pkt$team == t_b, "pkt"]) + 3

  # remis
  if(b_a == b_b) {
    tabela_pkt[tabela_pkt$team == t_a, "pkt"] <- as.integer(tabela_pkt[tabela_pkt$team == t_a, "pkt"]) + 1
    tabela_pkt[tabela_pkt$team == t_b, "pkt"] <- as.integer(tabela_pkt[tabela_pkt$team == t_b, "pkt"]) + 1
  }

  if(as.integer(sezon_2017_2018[n_mecz, "Data"]) != last_date) {
    # kolejna kolejka
    tabela_pkt_pos <- bind_rows(tabela_pkt_pos,
                                tabela_pkt %>%
                                  arrange(desc(pkt), desc(bwin-blost)) %>%
                                  select(team) %>%
                                  mutate(pos = row_number()) %>%
                                  mutate(data = as.integer(sezon_2017_2018[n_mecz, "Data"])))
    last_date <- as.integer(sezon_2017_2018[n_mecz, "Data"])
  }
}
################################

# tabela po 30 kolejce:
arrange(tabela_pkt, desc(pkt))

tabela_pkt_pos$data <- as.POSIXct("1970-01-01") + days(tabela_pkt_pos$data)

ggplot() +
  geom_line(data = tabela_pkt_pos,
            aes(data, 16-pos, color=team)) +
  geom_text(data = filter(tabela_pkt_pos, data==max(data)),
            aes(data+5*3600*24, 16-pos, color=team, label=team), hjust=0) +
  expand_limits(x = c(min(tabela_pkt_pos$data),
                      max(tabela_pkt_pos$data)+100*3600*24)) +
  theme_minimal() +
  theme(legend.position = "none")
