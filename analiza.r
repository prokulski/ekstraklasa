setwd("~/RProjects/ekstraklasa")

library(tidyverse)
library(lubridate)

mecze <- readRDS("mecze.RDS")

theme_set(theme_minimal())

# średnia ze wszystkich meczy pomiędzy
wynik_gosp_gosc <- mecze %>%
  group_by(gosp, gosc) %>%
  summarise(mb_gosc = mean(b_gosc), mb_gosp = mean(b_gosp)) %>%
  ungroup() %>%
  mutate(wygrana_gospodarza = ifelse(mb_gosp>mb_gosc, "Gospodarz",
                                     ifelse(mb_gosp==mb_gosc, "Remis",
                                            "Gość")))

wynik_gosp_gosc %>%
  ggplot() +
  geom_tile(aes(gosc, gosp, fill=wygrana_gospodarza), color="black") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Gość", y="Gospodarz", fill="Zwycięzca")

round(100 * prop.table(table(wynik_gosp_gosc$wygrana_gospodarza)), 1)

# kto najczęściej wygrywa
wygrani <- mecze %>%
  mutate(wygrany = ifelse(b_gosp>b_gosc, gosp,
                          ifelse(b_gosp==b_gosc, "Remis",
                                 gosc)),
         przegrany = ifelse(b_gosp>b_gosc, gosc,
                            ifelse(b_gosp==b_gosc, "Remis",
                                   gosp)))

wygrani %>%
  count(wygrany) %>%
  ungroup() %>%
  filter(wygrany != "Remis") %>%
  arrange(n) %>%
  mutate(wygrany = factor(wygrany, levels = wygrany)) %>%
  ggplot() +
  geom_bar(aes(wygrany, n), fill="lightgreen", color = "black", stat="identity") +
  geom_text(aes(wygrany, n, label = n), hjust = 1.3) +
  coord_flip() +
  labs(x = "Drużyna", y = "Liczba wygranych meczy")


wygrani %>%
  count(przegrany) %>%
  ungroup() %>%
  filter(przegrany != "Remis") %>%
  arrange(n) %>%
  mutate(przegrany = factor(przegrany, levels = przegrany)) %>%
  ggplot() +
  geom_bar(aes(przegrany, n), fill="lightgreen", color = "black", stat="identity") +
  geom_text(aes(przegrany, n, label = n), hjust = 1.3) +
  coord_flip() +
  labs(x = "Drużyna", y = "Liczba przegranych meczy")


# najczęstrze wyniki - wg sezonu
mecze %>%
  mutate(b_a = ifelse(b_gosp>b_gosc, b_gosp,
                      ifelse(b_gosp<b_gosc, b_gosc, b_gosc)),
         b_b = ifelse(b_gosp>b_gosc, b_gosc,
                      ifelse(b_gosp<b_gosc, b_gosp, b_gosp))) %>%
  count(sezon, b_a, b_b) %>%
  ungroup() %>%
  group_by(sezon) %>%
  mutate(p=100*n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(b_a, b_b, fill=p), color="black") +
  geom_text(aes(b_a, b_b, label=paste0(round(p, 1), "%")), color="white") +
  labs(x="Bramki A", y = "Bramki B", fill = "Częstość") +
  facet_wrap(~sezon)

# liczba bramek na mecz wg sezonu
mecze %>%
  group_by(sezon) %>%
  summarise(bramki = sum(b_gosc)+sum(b_gosp),
            n=n()) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(sezon, bramki/n), fill="lightgreen", color = "black", stat="identity") +
  geom_text(aes(sezon, bramki/n, label=round(bramki/n, 2)), vjust = 1.2) +
  labs(x = "Sezon", y = "Średnia liczba bramek w meczu")


# historia wygranych i przegranych wg drużyny
historia <- bind_rows(wygrani %>%
                        select(sezon, data, druzyna=wygrany) %>%
                        mutate(d = "wygrany"),
                      wygrani %>%
                        select(sezon, data, druzyna=przegrany) %>%
                        mutate(d = "przegrany"),
                      wygrani %>%
                        filter(wygrany == "Remis") %>%
                        select(sezon, data, druzyna=gosc) %>%
                        mutate(d = "remis"),
                      wygrani %>%
                        filter(wygrany == "Remis") %>%
                        select(sezon, data, druzyna=gosp) %>%
                        mutate(d = "remis")) %>%
  filter(druzyna != "Remis")

historia_t <- historia %>%
  count(druzyna, d) %>%
  ungroup() %>%
  group_by(druzyna) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup()

historia_t_fct <- historia_t %>%
  filter(d=="wygrany") %>%
  arrange(p)

# wszystkie sezony
historia_t %>%
  mutate(druzyna = factor(druzyna, levels = historia_t_fct$druzyna)) %>%
  ggplot() +
  geom_bar(aes(druzyna, p, fill=d), stat="identity") +
  geom_hline(yintercept = c(20, 50, 75), color = "black") +
  labs(x="Drużyna", y="Udział procentowy", fill="") +
  coord_flip()

# sezon ostatni
historia %>%
  filter(sezon == "2016-17") %>%
  ggplot() +
  geom_point(aes(data, d, color=d)) +
  labs(x = "Data meczu", y="", color="") +
  facet_wrap(~druzyna)


# historia wygranych i przegranych meczy wg drużyny po sezonach
historia %>%
  count(sezon, druzyna, d) %>%
  ungroup() %>%
  group_by(sezon, druzyna) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  mutate(sezon = factor(sezon, levels=sort(unique(sezon), decreasing = TRUE))) %>%
  ggplot() +
  geom_bar(aes(sezon, p, fill=d), stat="identity") +
  geom_hline(yintercept = c(20, 50, 75), color = "black") +
  labs(x="Sezon", y="", fill="") +
  coord_flip() +
  facet_wrap(~druzyna)
