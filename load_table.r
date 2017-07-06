setwd("~/RProjects/ekstraklasa")

library(tidyverse)
library(rvest)
library(lubridate)

rm(list=ls())

# linki do stron z wynikami poszczególnych sezonów
sezony <- data_frame(sezon_url = c("http://www.hppn.pl/liga/sezon-po-sezonie/87,sezon-2008-09/537,Ekstraklasa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/88,sezon-2009-10/554,Ekstraklasa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/89,sezon-2010-11/558,Ekstraklasa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/90,sezon-2011-12/562,Ekstraklasa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/97,sezon-2012-13/577,Ekstraklasa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/98,sezon-2013-14/581,Ekstraklasa-faza-zasadnicza/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/98,sezon-2013-14/581,Ekstraklasa-grupa-mistrzowska/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/98,sezon-2013-14/581,Ekstraklasa-grupa-spadkowa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/99,sezon-2014-15/588,Ekstraklasa-faza-zasadnicza/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/99,sezon-2014-15/588,Ekstraklasa-grupa-mistrzowska/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/99,sezon-2014-15/588,Ekstraklasa-grupa-spadkowa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/100,sezon-2015-16/592,Ekstraklasa-faza-zasadnicza/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/100,sezon-2015-16/592,Ekstraklasa-grupa-mistrzowska/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/100,sezon-2015-16/592,Ekstraklasa-grupa-spadkowa/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/101,sezon-2016-17/597,Ekstraklasa-faza-zasadnicza/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/101,sezon-2016-17/597,Ekstraklasa-grupa-mistrzowska/rezultaty",
                                   "http://www.hppn.pl/liga/sezon-po-sezonie/101,sezon-2016-17/597,Ekstraklasa-grupa-spadkowa/rezultaty"),
                     sezon_str = c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2013-14", "2013-14",
                                   "2014-15", "2014-15", "2014-15", "2015-16", "2015-16", "2015-16", "2016-17", "2016-17", "2016-17"))

# pusta ramka na dane
mecze <- data_frame()

# dla każdego sezonu pobierz dane ze strony
for(i in 1:nrow(sezony)) {
  page <- read_html(as.character(sezony[i, "sezon_url"]))

  tabela <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    .[-1,-5] %>%
    filter(substr(X1, 1, 7) != "kolejka") %>%
    mutate(data = dmy(X1)) %>%
    separate(X3, c("b_gosp", "b_gosc"), sep = ":") %>%
    mutate(sezon = as.character(sezony[i, "sezon_str"])) %>%
    select(sezon, data, gosp=X2, b_gosp, gosc=X4, b_gosc) %>%
    mutate(b_gosp = as.integer(b_gosp), b_gosc = as.integer(b_gosc)) %>%
    mutate(b_gosp = ifelse(is.na(b_gosp), 0, b_gosp),
           b_gosc = ifelse(is.na(b_gosc), 0, b_gosc))

    mecze <- bind_rows(mecze, tabela)
}

# zapisz plik lokalnie
saveRDS(mecze, file = "mecze.RDS")

