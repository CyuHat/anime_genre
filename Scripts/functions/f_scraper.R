# Librairies----
library(httr)
library(rvest)
library(purrr)
library(dplyr)

# Numérote les pages----
make.pages <- function(url,num) {
  return(paste0(url,num))
}

# Crée les pages----
make.urls <- function(){
  url <- "https://www.anime-planet.com/anime/all?page="
  
  urls <- map2(url,1:530,make.pages)
  
  return(urls)
}

# Collecte les liens des animes----
collect.links <- function(urls) {
  page <- read_html(urls)
  
  page_urls <- 
    page %>% 
    html_nodes("li.card a") %>% 
    html_attr("href") %>% 
    url_absolute("https://www.anime-planet.com/anime")
  
  Sys.sleep(3)
  
  return(page_urls)
}

# Collecte tous les liens des animes----
# Sauvegarde le résultat
collect.all.links <- function(urls){
  page_urls <- map(urls,collect.links)
  
  page_urls <- flatten(page_urls)
  
  save(page_urls, urls, file="Data/liste_anime_planet.Rda")
  
  return(page_urls)
}

# Collecte les données nécessaires pour un anime----
collect.data <- function(page_urls, i) {
  anime_page <- read_html(page_urls)
  
  nom <- 
    anime_page %>% 
    html_node("h1") %>% 
    html_text2()
  
  format <-
    anime_page %>% 
    html_node("span.type") %>% 
    html_text2()
  
  studio <-
    anime_page %>% 
    html_node("div.md-1-5:nth-child(2)") %>% 
    html_text2()
  
  annee <-
    anime_page %>% 
    html_node("span.iconYear") %>% 
    html_text2()
  
  vote <-
    anime_page %>% 
    html_node(".avgRating > span:nth-child(1)") %>% 
    html_text2()
  
  rang <-
    anime_page %>% 
    html_node("div.pure-1:nth-child(5)") %>% 
    html_text2()
  
  tags <-
    anime_page %>% 
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

# Collecte les tags d'un anime----
collect.tags <- function(lien_tags){
  p <- read_html(l) 
  
  tags2 <-
    p %>% 
    html_nodes("div.tags") %>% 
    html_text2() %>% 
    stringr::str_remove_all("Tags\n") %>% 
    stringr::str_replace_all("\n",";")
  
  a <- tibble::tibble(tags2=tags2,
                      lien=l)
  
  text <- paste(i, "sur 5'582")
  
  print(text)
  
  i <<- i+1
  
  Sys.sleep(3)
  
  return(a)
}

# Collecte les données pour tous les animes-----
collect.all.data <- function(page_urls){
  i = 1
  
  manga_genre <- map_dfr(page_urls, collect.data, i)
  
  liens_tags <-
    manga_genre %>% 
    filter(is.na(tags)) %>% 
    pull(lien)
  
  i = 1
  
  all_tags <- map_dfr(liens_tags, collect.tags)
  
  manga_genre <- left_join(manga_genre, all_tags)
  
  manga_genre <-
    manga_genre %>% 
    mutate(tags = ifelse(is.na(tags),tags2,tags)) %>% 
    select(-tags2)
  
  save(manga_genre, file="manga_genre.Rda")
  
  return(manga_genre)
}

# Fait la collecte complète----
whole.collect <- function(){
  urls <- make.urls()
  
  page_urls <- collect.all.links(urls)
  
  animes <- collect.all.data(page_urls)
  
  return(animes)
}