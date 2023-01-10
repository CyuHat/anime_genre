# Préparation----
library(stringr)
library(tidyr)

# Récupération des fonctions----
source("Scripts/functions/f_scraper.R")


# Collecte-----
# /!\ La collecte prend 42 heures
# manga_genre <- whole.collect()


# Nettoyage-----
# mangas
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

# Manga tags
mangas_tags <- 
  mangas %>% 
  separate_rows(tags,sep=";") %>% 
  mutate(tags = str_remove_all(tags,","))

# Sauvegarde
save(mangas, file="MyData/mangas_clean.Rda")

save(mangas_tags, file="MyData/mangas_tags.Rda")

write.csv(mangas, file = "MyData/mangas.csv",
          row.names = FALSE, sep = ";", fileEncoding = "UTF-8")

# Tout supprimer----
rm(list = ls())