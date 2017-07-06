setwd("~/RProjects/ekstraklasa")

library(tidyverse)
library(lubridate)

rm(list=ls())

mecze <- readRDS("mecze.RDS")
mecze <- mecze %>% mutate(n = row_number())

lista_druzyn <- bind_rows(mecze %>%
                            distinct(gosp) %>%
                            rename(druzyna = gosp),
                          mecze %>%
                            distinct(gosc) %>%
                            rename(druzyna = gosc)) %>%
  distinct() %>%
  .$druzyna


# ELO
# Założenia:
# Wszystkie drużyny startują z jednakowego poziomu elo = 100
# rozgrywamy po 10 wirtualnych meczy
# wygrywa ten, kto wygrał najczęściej w symulacji 10 meczy
# losujemy wynik, tak żeby wygrany rzeczywiście wygrał:
# bramki wygranego = średnia liczba bramek wygranego w jego wygranych meczach
# bramki przegranego = średnia liczba bramek przegranego w jego przegranych meczach
# po każdym wirtualnym meczu liczymy nowy ranking

################################
##### FUNKCJE ELO           ####
################################

f <- function(Delta) {
  return (1 / (1 + 10^(Delta/400)))
}

findWinner <- function(team_a, team_b, elo_a, elo_b) {
  # teoria:
  # f(Delta) = 1 / (1 + 10^(Delta/400))
  # P(WhiteWins) = f(eloBlack - eloWhite - eloAdvantage + eloDraw)
  # P(BlackWins) = f(eloWhite - eloBlack + eloAdvantage + eloDraw)
  # P(Draw) = 1 - P(WhiteWins) - P(BlackWins)
  # eloAdvantage indicates the advantage of playing first (home team)
  # eloAdvantage = 32.8 +/- 4
  # eloDraw indicates how likely draws are.
  # eloDraw = 97.3 +/- 2

  # eloAdvantage <- rnorm(1, 32.8, 4) # eloAdvantage = 32.8 +/- 4
  eloAdvantage <- rnorm(1, 61, 1)
  eloDraw <- rnorm(1, 97.3, 2) # eloDraw = 97.3 +/- 2

  # wygrywa team.a - gospodarz
  p_a <- f(elo_b - elo_a - eloAdvantage + eloDraw)

  # wygrywa team.b - gość
  p_b <- f(elo_a - elo_b + eloAdvantage + eloDraw)

  # remis
  p_d <- 1 - p_a - p_b

  #    cat(paste0(team_a, " (", elo_a, ") - ", team_b, " (", elo_b, ")\n"))
  #    cat(paste0("\nPrawdopodobieństwo:\n" ,
  #               "\twygrywa ", team_a, " - ", round(100*p_a, 2), "%", "\n",
  #               "\twygrywa ", team_b, " - ", round(100*p_b, 2), "%", "\n",
  #               "\tremis - ", round(100*p_d, 2), "%\n\n"))

  # które prawdopodobieństwo największe?
  if(p_a > p_b) win_team <- team_a
  if(p_b > p_a) win_team <- team_b
  if(p_a == p_b) win_team <- "remis"
  if((p_d > p_a) & (p_d > p_b)) win_team <- "remis"

  # i pchamy do tabeli wszystkiego
  return(win_team)
}

calcNewElo <- function(score_a, score_b, elo_a, elo_b) {
  # teoria:
  # Rn = Ro + K × (W - We)
  # Rn is the new rating, Ro is the old (pre-match) rating.
  # K is the weight constant for the tournament played:
  # 60 for World Cup finals;
  # 50 for continental championship finals and major intercontinental tournaments;
  # 40 for World Cup and continental qualifiers and major tournaments;
  # 30 for all other tournaments;
  # 20 for friendly matches.
  # K is then adjusted for the goal difference in the game.
  # It is increased by half if a game is won by two goals,
  # by 3/4 if a game is won by three goals,
  # and by 3/4 + (N-3)/8 if the game is won by four or more goals, where N is the goal difference.
  # W is the result of the game (1 for a win, 0.5 for a draw, and 0 for a loss).
  # We is the expected result (win expectancy), either from the chart or the following formula:
  # We = 1 / (10^(-dr/400) + 1)

  K <- 30

  score_diff <- score_a - score_b
  if(score_diff == 2) K <- K * 1.5
  if(score_diff == 3) K <- K * 1.75
  if(score_diff > 3) K <- K * 1.75 + (score_diff-3)/8

  # W is the result of the game (1 for a win, 0.5 for a draw, and 0 for a loss).
  # roznica w bramkach
  if(score_a > score_b) W <- 1
  if(score_a < score_b) W <- 0
  if(score_a == score_b) W <- 0.5

  elo_diff <- elo_a - elo_b
  We <- 1/( (10^(-elo_diff/400)) + 1)
  newelo_a <- round(elo_a + K*(W-We),0)

  elo_diff <- newelo_a - elo_a
  newelo_b <- elo_b - elo_diff
  return(c(newelo_a, newelo_b))
}
################################



################################
##### WIRTUALNE MECZE       ####
################################

historyMatches <- function(team_a_name, team_b_name) {
  # tabela meczy drużyn grających razem
  # na podstawie prawdziwych wyników
  sym_mecze <- mecze %>%
    #      filter(n < n_mecz) %>% # tylko mecze, które były wcześniej
    select(-sezon, -data) %>%
    filter(gosp %in% c(team_a_name, team_b_name) & gosc %in% c(team_a_name, team_b_name)) %>%
    mutate(teamA = team_a_name, teamB = team_b_name) %>%
    mutate(b_A = ifelse(gosp == teamA, b_gosp, b_gosc),
           b_B = ifelse(gosp == teamB, b_gosp, b_gosc)) %>%
    select(teamA, b_A, teamB, b_B)

  return(sym_mecze)
}

playMatch <- function(team_a, team_b, elo_a, elo_b, n_repeats = 100) {
  # funkcja rozgrywa n_repeats wirtualnych meczy między drużynami o podanych
  # nazwach i z podanym rankingiem ELO
  # zwraca wynik meczu

  # liczniki zwyciestwa i remisu
  a <- 0
  b <- 0
  r <- 0

  # losowanie zwycięzcy
  for(i in 1:n_repeats) {
    t <- findWinner(team_a, team_b, elo_a, elo_b)
    if(t==team_a) a <- a + 1
    if(t==team_b) b <- b + 1
    if(t=="remis") r <- r + 1
  }

  # podsumowanie losowania zwycięzcy
  if(deb_prn)
  {
    cat(paste0("Wyniki:\n\tWygrywa ", team_a, ": ", a,
               "\n\tWygrywa ", team_b, ": ", b,
               "\n\tRemis: ", r, "\n"))
  }

  # wylicz wynik meczu:
  # historyczne mecze
  sym_mecze <- historyMatches(team_a, team_b)

  # wygrywa A
  if(max(a, b, r) == a) {
    # wygrane mecze A
    tmp_score <- sym_mecze %>% filter(b_A > b_B) %>% select(b_A, b_B)
    if(nrow(tmp_score) > 0 ) {
      # wynik dla przegranego B - średnia z bramek B
      score_a <- sample(min(tmp_score$b_A):max(tmp_score$b_A), 1000, replace = TRUE) %>% mean() %>% round()

      # wynik dla wygranego A - średnia z bramek A
      score_b <- sample(min(tmp_score$b_B):max(tmp_score$b_B), 1000, replace = TRUE) %>% mean() %>% round()
    } else {
      # nie było takiej sytuacji wcześniej - losujemy wyniki, wygrana 1 punktem
      score_b <- sample(0:3, 1000, replace = TRUE) %>% mean %>% round
      score_a <- score_b + 1
    }
  }

  # wygrywa B
  if(max(a, b, r) == b) {
    # wygrane mecze B
    tmp_score <- sym_mecze %>% filter(b_B > b_A) %>% select(b_A, b_B)

    if(nrow(tmp_score) > 0 ) {
      score_a <- sample(min(tmp_score$b_A):max(tmp_score$b_A), 1000, replace = TRUE) %>% mean() %>% round()
      score_b <- sample(min(tmp_score$b_B):max(tmp_score$b_B), 1000, replace = TRUE) %>% mean() %>% round()
    } else {
      # nie było takiej sytuacji wcześniej - losujemy wyniki, wygrana 1 punktem
      score_a <- sample(0:3, 1000, replace = TRUE) %>% mean %>% round
      score_b <- score_a + 1
    }
  }

  # remis
  if(max(a, b, r) == r) {
    # mecze z remisem
    tmp_score <- sym_mecze %>% filter(b_A == b_B) %>% .$b_A
    # bramki dla obu takie same = średnia z remisów
    score_a <- sample(min(tmp_score):max(tmp_score), 1000, replace = TRUE) %>% mean() %>% round()
    score_b <- score_a
  }

  return(c(score_a, score_b))
}
################################



################################
#### SYMULACJA - PARAMETRY  ####
################################

deb_prn <- FALSE

# początkowe ELO = 1000 dla wszystkich
# elo_tab <- data_frame(team = lista_druzyn, elo = rep(1000, length(lista_druzyn)))

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
  tmp_score <- playMatch(team_a, team_b, elo_a, elo_b, 1000)
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
