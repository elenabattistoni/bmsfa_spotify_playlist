# ==============================================================================
# Preprocessing dei dati
# ==============================================================================

## Nota sulla riproducibilità

#Questo repository implementa un'unica pipeline di preprocessing standardizzata
#applicata in modo coerente sia all'analisi esplorativa sia alla stima del modello.
#Il dataset grezzo e le variabili analizzate sono quelli utilizzati nel lavoro
#di tesi. I risultati numerici possono differire da quelli riportati nella tesi.

library(tidyverse)
library(recipes)

dati <- as_tibble(read_csv("spotify_songs.csv"))

##preprocessing e pulizia
na_righe <- dati %>% filter(if_any(everything(), is.na)) %>% nrow()
perc_na_righe <- na_righe/ nrow(dati) *100

colSums(is.na(dati))
dati <- drop_na(dati)

dati <- dati %>% group_by(track_id, playlist_id) %>% slice(1) %>% ungroup()

#anno uscita (ha formati diversi) -> età della canzone
dati <- dati %>% 
  mutate(track_album_release_year = 
           year(parse_date_time(track_album_release_date,
                                orders = c("Ymd", "Y", "Ym"))), 
         age = 2026 - track_album_release_year,
         .keep = "unused")

#key e mode -> luminescence
mappa <- c("8" = 1, "3" = 2, "10" = 3, "5" = 4, "0" = 5, "7" = 6, 
           "2" = 7, "9" = 8, "4" = 9, "11" = 10, "6" = 11, "1" = 12)

dati <- dati %>% mutate(luminescence = as.numeric(mappa[as.character(key)]) 
                        + (mode * 12), .keep = "unused")

## selezione del campione
set.seed(1234)
playlist <- dati %>% 
  group_by(playlist_id, playlist_name, playlist_genre) %>% 
  summarise(n_songs = n(), .groups = "drop") %>% filter(n_songs == 100) %>% 
  filter(str_detect(tolower(playlist_name), tolower(playlist_genre)))

playlist_6 <- playlist %>% group_by(playlist_genre) %>% slice_sample(n = 1)

finale <- dati %>% filter(playlist_id %in% playlist_6$playlist_id)

variabili <- c("track_popularity", "age", "duration_ms", 
               "danceability", "energy", "loudness", "valence", 
               "tempo", "speechiness", "acousticness", "liveness", 
               "instrumentalness", "luminescence")

playlists <- playlist_6$playlist_name
risultati <- vector("list", 6)

##trasformazione di yeo-johnson e normalizzazione
for (i in 1:6){
  df_playlist <- finale %>% filter(playlist_name == playlists[i])
  
  rec <- recipe(~ ., data = df_playlist) %>%
    step_YeoJohnson(all_of(variabili)) %>%
    step_normalize(all_of(variabili))
  
  rec_prep <- prep(rec, training = df_playlist)
  
  risultati[[i]] <- bake(rec_prep, new_data = NULL)
}

finale_yj <- bind_rows(risultati)

#costruzione dataset
dataset_finale <- finale_yj %>% select(playlist_id, playlist_name, 
                            playlist_genre, all_of(variabili))

write_csv(dataset_finale, "dataset_finale_tesi_yj.csv")

dataset_finale_grezzo <- finale %>% select(playlist_id, playlist_name, 
                                       playlist_genre, all_of(variabili))

write_csv(dataset_finale_grezzo, "dataset_finale_grezzo.csv")