# Préparation----
library(httr)
library(rvest)
library(purrr)

url <- "https://www.anime-planet.com/anime/all?page="

les_pages <- function(url,num) {
  return(paste0(url,num))
}

urls <- map2(url,1:530,les_pages)

collect_lien <- function(urls) {
  page <- read_html(urls)
  
  liens <- 
    page %>% 
    html_nodes("li.card a") %>% 
    html_attr("href") %>% 
    url_absolute("https://www.anime-planet.com/anime")
  
  Sys.sleep(3)
  
  return(liens)
}

res <- map(urls,collect_lien)

resultat <- flatten(res)

resultat

save(resultat,res,url,file="liste_anime_planet.Rda")

url2 <- "https://www.anime-planet.com/anime/only-yesterday"

i = 1

collect_data <- function(url2) {
  page2 <- read_html(url2)
  
  nom <- 
    page2 %>% 
    html_node("h1") %>% 
    html_text2()
  
  format <-
    page2 %>% 
    html_node("span.type") %>% 
    html_text2()
  
  studio <-
    page2 %>% 
    html_node("div.md-1-5:nth-child(2)") %>% 
    html_text2()
  
  annee <-
    page2 %>% 
    html_node("span.iconYear") %>% 
    html_text2()
  
  vote <-
    page2 %>% 
    html_node(".avgRating > span:nth-child(1)") %>% 
    html_text2()
  
  rang <-
    page2 %>% 
    html_node("div.pure-1:nth-child(5)") %>% 
    html_text2()
  
  tags <-
    page2 %>% 
    html_nodes("div.tags:nth-child(2)") %>% 
    html_text2() %>% 
    stringr::str_remove_all("Tags\n") %>% 
    stringr::str_replace_all("\n",";")
  
  anime <- tibble::tibble(nom=nom,
                      format=format,
                      studio=studio,
                      annee=annee,
                      vote=vote,
                      rang=rang,
                      tags=tags[1],
                      lien=url2)
  
  text <- paste(i, "sur 18'519")
  
  print(text)
  
  i <<- i+1
  
  Sys.sleep(3)
  
  return(anime)
}

tagz <- function(l) {
  p <- read_html(l) 
  
  tags2 <-
    p %>% 
    html_nodes("div.tags") %>% 
    html_text2() %>% 
    stringr::str_remove_all("Tags\n") %>% 
    stringr::str_replace_all("\n",";")
  
  a <- tibble::tibble(tags2=tags2,lien=l)
  
  text <- paste(i, "sur 5'582")
  
  print(text)
  
  i <<- i+1
  
  Sys.sleep(3)
  
  return(a)
}

# Collecte-----
i = 1

manga_genre <- map_dfr(resultat,collect_data)

library(dplyr)

liens_tags <-
  manga_genre %>% 
  filter(is.na(tags)) %>% 
  pull(lien)

i = 1

tags2 <- map_dfr(liens_tags,tagz)

manga_genre <- left_join(manga_genre,tags2)

manga_genre <-
  manga_genre %>% 
  mutate(tags = ifelse(is.na(tags),tags2,tags)) %>% 
  select(-tags2)

save(manga_genre,file="manga_genre.Rda")

# Nettoyage-----
library(stringr)
library(tidyr)

mangas <- 
  manga_genre %>% 
  separate(col=format,into=c("format","episodes") ,sep=" \\(") %>%
  separate(col=vote,into=c("note","out","of","n5","from",
                           "popularite","votes"),
           sep=" ") %>% 
  select(-out,-of,-n5,-from,-votes) %>% 
  mutate(annee = str_replace_all(annee,"(\\d{4}).*","\\1"),
         annee = as.numeric(annee),
         episodes = str_replace_all(episodes,"(\\d{1,4}).*","\\1"),
         episodes = as.numeric(episodes),
         note = as.numeric(note),
         popularite = as.numeric(str_remove_all(popularite,",")),
         rang = str_remove_all(rang,"Rank \\#"),
         rang = str_remove_all(rang,","),
         rang = as.numeric(rang),
         tags = tolower(tags))
  
mangas <-
  mangas %>% 
  mutate(episodes = ifelse(is.na(episodes),1,episodes),
         annee = ifelse(is.na(annee),2023,annee),
         popularite = ifelse(is.na(popularite),note,popularite),
         note = ifelse(is.na(popularite),2.5,note),
         rang = ifelse(is.na(rang),15378,rang)) %>% 
  distinct() %>% 
  mutate(periode = case_when(
    annee %in% 1907:2001 ~ "1907-2001",
    annee %in% 2002:2011 ~ "2002-2011",
    annee %in% 2012:2016 ~ "2012-2016",
    annee %in% 2017:2023 ~ "2017-2023"
  ))

save(mangas,file="mangas_clean.Rda")

mangas_tags <- 
  mangas %>% 
  separate_rows(tags,sep=";") %>% 
  mutate(tags = str_remove_all(tags,","))

save(mangas_tags,file="mangas_tags.Rda")

# Analyse-----
library(ggplot2)
theme_set(theme_bw())

# Évolution du nombre d'animes
mangas %>% 
  filter(!str_detect(tags,"content"),
         annee<2022) %>% 
  count(annee,sort=TRUE) %>% 
  ggplot(aes(annee,n)) +
  geom_area(fill="cyan",alpha=0.4) +
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(1907,2024,9)) +
  labs(title="Évolution du nombre d'animes par année",
       subtitle="Pic en 2017 (973)",
       y=NULL) +
  geom_vline(xintercept = 2017, color="red",
             linetype = "dashed") +
  geom_hline(yintercept = 973, color="red",
             linetype = "dashed")

# Top 10: Genre les plus courants
mangas_tags %>% 
  drop_na() %>% 
  mutate(tags = str_wrap(tags,15)) %>% 
  count(tags,sort=TRUE) %>% 
  top_n(10,n) %>% 
  ggplot(aes(reorder(tags,n),n)) +
  geom_col(fill="red",alpha=0.6,color="black") +
  coord_flip() +
  labs(title="Les 10 genres les plus courant dans les animes",
       subtitle="Toute année confondue",
       x=NULL, y=NULL)

# # Top 10: Genre les plus courants par période
mangas_tags %>%
  drop_na() %>% 
  mutate(annee = case_when(
    annee %in% 1907:2001 ~ "1907-2001",
    annee %in% 2002:2011 ~ "2002-2011",
    annee %in% 2012:2016 ~ "2012-2016",
    annee %in% 2017:2023 ~ "2017-2023"
  ),
  tags = str_wrap(tags,15)) %>% 
  group_by(annee) %>% 
  count(tags) %>% 
  top_n(10,n) %>% 
  ggplot(aes(tidytext::reorder_within(tags,n,annee),n,fill=annee)) +
  geom_col(color="black") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(~annee, scales="free") +
  theme(legend.position = "none") +
  labs(title="Les 10 genres d'animes les plus courants par période",
       x=NULL, y=NULL)

# Évolution du genre shoujo
mangas_tags %>% 
  filter(tags=="shoujo" & annee<2022) %>%
  group_by(annee) %>% 
  count(tags) %>% 
  ggplot(aes(annee,n)) +
  geom_area(fill="yellow",alpha=0.4) +
  geom_line() +
  scale_x_continuous(breaks=seq(1910,2025,5)) +
  geom_hline(yintercept = 29, color="red",
             linetype="dashed") +
  geom_vline(xintercept = 2016, color="red",
             linetype="dashed") +
  labs(title="Évolution du nombre d'animes shoujo",
       subtitle = "Pic en 2016 (29)",
       x=NULL, y=NULL)

# Évolution des 4 genres classiques   
mangas_tags %>% 
  group_by(annee) %>% 
  count(tags) %>% 
  filter(tags %in% c("shoujo","shounen","josei","seinen")) %>% 
  ggplot(aes(annee,n,color=tags)) +
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(1960,2025,5)) +
  theme(legend.position = "top") +
  labs(title="Évolution des quatre genres classiques d'animes dans le temps",
       color="Genre",
       x=NULL, y=NULL)

# Évolution des 4 genres classiques base 100
mangas_tags %>% 
  group_by(annee) %>% 
  count(tags) %>% 
  filter(tags %in% c("shoujo","shounen","josei","seinen"),
         annee>1989) %>% 
  ungroup() %>% 
  group_by(tags) %>% 
  mutate(base_100 = (n/first(n))*100) %>% 
  ggplot(aes(annee,base_100,color=tags)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(1990,2025,5)) +
  scale_y_continuous(breaks=seq(0,1000,100)) +
  theme(legend.position = "top") +
  labs(title="Évolution des quatre genres classiques d'animes dans le temps",
       subtitle="En base 100",
       color="Genre",
       x=NULL, y=NULL)

# Évolution de la part des 4 genres principaux
mangas_tags %>% 
  group_by(annee) %>% 
  count(tags) %>% 
  filter(tags %in% c("shoujo","shounen","josei","seinen"),
         annee>1989) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(annee,prop,fill=tags)) +
  geom_hline(yintercept = c(0.25,0.5,0.75),
             linetype="dashed") +
  geom_area(alpha=0.4) +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=seq(1990,2025,5)) +
  labs(title="Évolution de la part des quatre genres classiques",
       x=NULL, y=NULL, fill="Genre")

library(widyr)

mangas_corr <-
  mangas_tags %>% 
  group_by(annee) %>% 
  count(tags) %>% 
  pairwise_cor(tags,annee,n)

# Corrélation des tags selon leur évolution pour les 4 genres classiques
mangas_corr %>% 
  filter(item1 %in% c("shounen","shoujo","seinen","josei")) %>%
  group_by(item1) %>% 
  arrange(desc(correlation)) %>% 
  mutate(correlation = round(correlation,3)) %>% 
  top_n(10,correlation) %>% 
  ggplot(aes(tidytext::reorder_within(item2,correlation,item1),
             correlation,fill=item1)) +
  geom_col(alpha=0.4) +
  geom_label(aes(label=correlation)) +
  tidytext::scale_x_reordered()+
  coord_flip() +
  facet_wrap(~item1,scales="free") +
  theme(legend.position = "none") +
  labs(title="Les tags les plus corrélés aux quatre genres d'animes classiques",
       x=NULL, y=NULL)

mangas_cooc <- 
  mangas_tags %>% 
  pairwise_cor(tags,nom)

# Corrélation (concurrence) des tags avec les 4 genres classiques
mangas_cooc %>% 
  filter(item1 %in% c("shounen","shoujo","seinen","josei")) %>%
  group_by(item1) %>% 
  top_n(10,correlation) %>% 
  mutate(correlation = round(correlation,3),
         item2 = str_wrap(item2,15)) %>% 
  ggplot(aes(tidytext::reorder_within(item2,correlation,item1),
             correlation,
             fill=item1)) +
  geom_hline(yintercept = 0.25, color="red",
             linetype="dashed") +
  geom_col(color="black") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(~item1, scales="free") +
  theme(legend.position = "none") +
  labs(title="Tags les plus corrélés aux quatre genres classiques",
       subtitle="Basé sur la coocurence",
       x=NULL, y=NULL,
       caption="Compare le nombre de fois où les tags\napparaissent ensemble contre le nombre\nde fois où ils n'aparraissent pas ensemble")

# Absolute value of r 	Strength of relationship
# r < 0.25 	No relationship
# 0.25 < r < 0.5 	Weak relationship
# 0.5 < r < 0.75 	Moderate relationship
# r > 0.75 	Strong relationship

# Corrélation des genres borderlines
mangas_cooc %>% 
  filter(item1 %in% c("ecchi","smut","harem","reverse harem")) %>%
  mutate(item1 = factor(item1,c("ecchi","smut","harem","reverse harem"))) %>% 
  group_by(item1) %>% 
  top_n(10,correlation) %>% 
  mutate(correlation = round(correlation,3),
         item2 = str_wrap(item2,15)) %>% 
  ggplot(aes(tidytext::reorder_within(item2,correlation,item1),
             correlation,
             fill=item1)) +
  geom_hline(yintercept = 0.25, color="red",
             linetype="dashed") +
  geom_col(color="black") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(~item1, scales="free") +
  theme(legend.position = "none") +
  labs(title="Tags les plus corrélés au genres borderlines",
       subtitle="Basé sur la coocurence\necchi, smut, harem, reverse harem",
       x=NULL, y=NULL,
       caption="Compare le nombre de fois où les tags\napparaissent ensemble contre le nombre\nde fois où ils n'aparraissent pas ensemble")

# Corrélation des genre sexuel
mangas_cooc %>% 
  filter(str_detect(item1,"sex")) %>%
  group_by(item1) %>% 
  top_n(10,correlation) %>% 
  mutate(correlation = round(correlation,3),
         item2 = str_wrap(item2,15)) %>% 
  ggplot(aes(tidytext::reorder_within(item2,correlation,item1),
             correlation,
             fill=item1)) +
  geom_hline(yintercept = 0.25, color="red",
             linetype="dashed") +
  geom_col(color="black") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(~item1, scales="free") +
  theme(legend.position = "none") +
  labs(title="Tags les plus corrélés contenu sexuel",
       subtitle="Basé sur la coocurence",
       x=NULL, y=NULL,
       caption="Compare le nombre de fois où les tags\napparaissent ensemble contre le nombre\nde fois où ils n'aparraissent pas ensemble")

# Évolution du nombre de studio d'animation
mangas %>% 
  filter(!str_detect(tags,"content"),
         annee < 2022) %>% 
  group_by(annee) %>% 
  count(studio) %>% 
  ungroup() %>% 
  count(annee) %>% 
  ggplot(aes(annee,n)) +
  geom_area(alpha=0.4) +
  geom_line() +
  geom_hline(yintercept = 255, color="red",
             linetype="dashed") +
  geom_vline(xintercept = 2021, color="red",
             linetype="dashed") +
  labs(title="Évolution du nombre de studios d'animation",
       subtitle="Pic 2021 (255)",
       x=NULL, y=NULL)

  







  
