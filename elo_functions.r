## Skrypt zawierający funkcje pozwalające na rozegranie wirtualnego meczu
##
## Funkcje:
##   findWinner() - dla podanych parametrów na podstawie rankingu ELO drużyn zwraca zwycięzcę (nazwę drużyny)
##   calcNewElo() - wylicza nowe wartości ELO na podstawie podanego wyniku i aktualnych wartości ELO
##   historyMatches() - zwraca podzbiór tabeli "mecze" z rozgrywkami pomiędzy podanymi drużynami
##   playMatch() - rozgrywa wirtualny mecz pomiędzy drużynami
##


library(tidyverse)
library(lubridate)


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

findWinner <- function(team_a, team_b, elo_a, elo_b,
                       eloDraw = 0, eloAdvantage = 0) {
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
  if(eloAdvantage == 0) eloAdvantage <- rnorm(1, 61, 1)
  if(eloDraw ==0) eloDraw <- rnorm(1, 97.3, 2) # eloDraw = 97.3 +/- 2

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

playMatch <- function(team_a, team_b, elo_a, elo_b,
                      n_repeats = 100,
                      elo_draw, elo_home) {
  # funkcja rozgrywa n_repeats wirtualnych meczy między drużynami o podanych
  # nazwach i z podanym rankingiem ELO
  # zwraca wynik meczu

  # liczniki zwyciestwa i remisu
  a <- 0
  b <- 0
  r <- 0

  # losowanie zwycięzcy
  for(i in 1:n_repeats) {
    t <- findWinner(team_a, team_b, elo_a, elo_b, elo_draw, elo_home)
    if(t==team_a) a <- a + 1
    if(t==team_b) b <- b + 1
    if(t=="remis") r <- r + 1
  }

  # wylicz wynik meczu:
  # historyczne mecze
  sym_mecze <- historyMatches(team_a, team_b)

  # wygrywa A
  if(max(a, b, r) == a) {
    # wygrane mecze A
    tmp_score_df <- sym_mecze %>% filter(b_A > b_B) %>% select(b_A, b_B)

    # była taka sytuacja? korzystamy z niej
    if(nrow(tmp_score_df) > 0 ) {
      # wynik dla przegranego B - średnia z bramek B
      score_a <- sample(min(tmp_score_df$b_A):max(tmp_score_df$b_A), 1000, replace = TRUE) %>% mean() %>% round()

      # wynik dla wygranego A - średnia z bramek A
      score_b <- sample(min(tmp_score_df$b_B):max(tmp_score_df$b_B), 1000, replace = TRUE) %>% mean() %>% round()
    } else {
      # nie było takiej sytuacji wcześniej - losujemy wyniki, wygrana 1 punktem
      score_b <- sample(0:3, 1000, replace = TRUE) %>% mean %>% round
      score_a <- score_b + 1
    }
  }

  # wygrywa B
  if(max(a, b, r) == b) {
    # wygrane mecze B
    tmp_score_df <- sym_mecze %>% filter(b_B > b_A) %>% select(b_A, b_B)

    # była taka sytuacja? korzystamy z niej
    if(nrow(tmp_score_df) > 0 ) {
      score_a <- sample(min(tmp_score_df$b_A):max(tmp_score_df$b_A), 1000, replace = TRUE) %>% mean() %>% round()
      score_b <- sample(min(tmp_score_df$b_B):max(tmp_score_df$b_B), 1000, replace = TRUE) %>% mean() %>% round()
    } else {
      # nie było takiej sytuacji wcześniej - losujemy wyniki, wygrana 1 punktem
      score_a <- sample(0:3, 1000, replace = TRUE) %>% mean %>% round
      score_b <- score_a + 1
    }
  }

  # remis
  if(max(a, b, r) == r) {
    # mecze z remisem
    tmp_score_df <- sym_mecze %>% filter(b_A == b_B) %>% .$b_A
    if(length(tmp_score_df) > 0 ) {
      # bramki dla obu takie same = średnia z remisów
      score_a <- sample(min(tmp_score_df):max(tmp_score_df), 1000, replace = TRUE) %>% mean() %>% round()
      score_b <- score_a
    } else {
      # nie było takiej sytuacji wcześniej - losujemy wynik remisowy 0:0, 1:1 lub 2:2
      score_a <- sample(0:2, 1000, replace = TRUE) %>% mean %>% round
      score_b <- score_a
    }
  }

  return(c(score_a, score_b))
}
################################

