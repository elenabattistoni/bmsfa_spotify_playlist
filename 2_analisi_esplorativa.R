# ==============================================================================
# Analisi esplorativa
# ==============================================================================

library(tidyverse)
library(patchwork)

#dati grezzi
dati_grezzi <- read_csv("dataset_finale_grezzo.csv")

variabili <- c("track_popularity", "age", "duration_ms", 
               "danceability", "energy", "loudness", "valence", 
               "tempo", "speechiness", "acousticness", "liveness", 
               "instrumentalness", "luminescence")

#boxplot per playlist
myplot2 <- ggplot(dati_grezzi, aes(x = playlist_genre)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  labs(x = NULL, 
       y = NULL)

pl1 <- myplot2 + geom_boxplot(aes(y = track_popularity), col = "#08519C", 
                              outlier.colour = "#6BAED6")

pl2 <- myplot2 + geom_boxplot(aes(y = age), col = "#08519C", 
                              outlier.colour = "#6BAED6")

pl3 <- myplot2 + geom_boxplot(aes(y = duration_ms), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl4 <- myplot2 + geom_boxplot(aes(y = danceability), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl5 <- myplot2 + geom_boxplot(aes(y = energy), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl6 <- myplot2 + geom_boxplot(aes(y = loudness), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl7 <- myplot2 + geom_boxplot(aes(y = valence), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl8 <- myplot2 + geom_boxplot(aes(y = tempo), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl9 <- myplot2 + geom_boxplot(aes(y = speechiness), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl10 <- myplot2 + geom_boxplot(aes(y = acousticness), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

pl11 <- myplot2 + geom_boxplot(aes(y = liveness), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

pl12 <- myplot2 + geom_boxplot(aes(y = instrumentalness), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

pl13 <- myplot2 + geom_boxplot(aes(y = luminescence), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

##dati trasformati con yeo johnson 
dati_yj <- read_csv("dataset_finale_tesi_yj.csv")

#boxplot per playlist
myplot2 <- ggplot(dati_yj, aes(x = playlist_genre)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  labs(x = NULL, 
       y = NULL)

pl1_yj <- myplot2 + geom_boxplot(aes(y = track_popularity), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl2_yj <- myplot2 + geom_boxplot(aes(y = age), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl3_yj <- myplot2 + geom_boxplot(aes(y = duration_ms), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl4_yj <- myplot2 + geom_boxplot(aes(y = danceability), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl5_yj <- myplot2 + geom_boxplot(aes(y = energy), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl6_yj <- myplot2 + geom_boxplot(aes(y = loudness), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl7_yj <- myplot2 + geom_boxplot(aes(y = valence), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl8_yj <- myplot2 + geom_boxplot(aes(y = tempo), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl9_yj <- myplot2 + geom_boxplot(aes(y = speechiness), col = "#08519C", 
                              outlier.colour = "#6BAED6") 

pl10_yj <- myplot2 + geom_boxplot(aes(y = acousticness), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

pl11_yj <- myplot2 + geom_boxplot(aes(y = liveness), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

pl12_yj <- myplot2 + geom_boxplot(aes(y = instrumentalness), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

pl13_yj <- myplot2 + geom_boxplot(aes(y = luminescence), col = "#08519C", 
                               outlier.colour = "#6BAED6") 

#grafico finale (prima + dopo)
coppia_popularity <- (pl1 + pl1_yj) + 
  plot_annotation(title = "Track Popularity") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_popularity <-wrap_elements(panel = coppia_popularity)

coppia_age <- (pl2 + pl2_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Age") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_age <-wrap_elements(panel = coppia_age)

coppia_duration <- (pl3 + pl3_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Duration") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_duration <- wrap_elements(panel = coppia_duration)

coppia_danceability <- (pl4 + pl4_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Danceability") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_danceability <- wrap_elements(panel = coppia_danceability)

coppia_energy <- (pl5 + pl5_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Energy") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_energy <- wrap_elements(panel = coppia_energy)

coppia_loudness <- (pl6 + pl6_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Loudness") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_loudness <- wrap_elements(panel = coppia_loudness)

coppia_valence <- (pl7 + pl7_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Valence") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_valence <- wrap_elements(panel = coppia_valence)

coppia_tempo <- (pl8 + pl8_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Tempo") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_tempo <- wrap_elements(panel = coppia_tempo)

coppia_speechiness <- (pl9 + pl9_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Speechiness") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_speechiness <- wrap_elements(coppia_speechiness)

coppia_acousticness <- (pl10 + pl10_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Acousticness") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_acousticness <- wrap_elements(panel = coppia_acousticness)

coppia_liveness <- (pl11 + pl11_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Liveness") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_liveness <- wrap_elements(panel = coppia_liveness)

coppia_instrumentalness <- (pl12 + pl12_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Instrumentalness") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_instrumentalness <- wrap_elements(panel = coppia_instrumentalness)

coppia_luminescence <- (pl13 + pl13_yj) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Luminescence") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
coppia_luminescence <- wrap_elements(panel = coppia_luminescence)

grafico_completo <- (coppia_popularity |  coppia_age) / 
  (coppia_duration | coppia_danceability) /
  (coppia_energy | coppia_loudness) / 
  (coppia_valence | coppia_tempo) /
  (coppia_speechiness | coppia_acousticness) /
  (coppia_liveness | coppia_instrumentalness) /
  (coppia_luminescence | plot_spacer())
  
grafico_completo

##corrplot
#corrplot totale
palette_col <- colorRampPalette(c("#B2182B", "#F7F7F7", "#08519C"))
corrplot::corrplot(cor(dati_grezzi %>% select(-playlist_id, - playlist_name, 
                                              -playlist_genre)), 
                   tl.col = "#191414",
                   col = palette_col(200))

corrplot::corrplot(cor(dati_yj %>% select(-playlist_id, - playlist_name, 
                                          -playlist_genre)), 
                   tl.col = "#191414",
                   col = palette_col(200))

#corrplot per playlist
par(mfrow = c(3,2))
playlist_raw <- dati_grezzi %>% group_split(playlist_id) 
for (i in 1:6){
  raw_name <- as.character(as.character(unique(playlist_raw[[i]]$playlist_genre)[1]))
  mat <- playlist_raw[[i]] %>% select(-playlist_id, -playlist_name, -playlist_genre)
  print(raw_name)
  corrplot::corrplot(cor(mat),
                     mar = c(0, 0, 2, 0),
                     tl.col = "#191414",
                     tl.cex = 0.7,
                     col = palette_col(200))
  title(main = raw_name, cex.main = 2.5, line = -1)
}

playlist_yj <- dati_yj %>% group_split(playlist_id) 
for (i in 1:6){
  raw_name <- as.character(as.character(unique(playlist_yj[[i]]$playlist_genre)[1]))
  mat <- playlist_yj[[i]] %>% select(-playlist_id, -playlist_name, -playlist_genre)
  print(raw_name)
  corrplot::corrplot(cor(mat),
                     mar = c(0, 0, 2, 0),
                     tl.col = "#191414",
                     tl.cex = 0.7,
                     col = palette_col(200))
  title(main = str_trunc(raw_name, 20), cex.main = 2)
}
par(mfrow = c(1,1))