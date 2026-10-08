# ==============================================================================
# Selezione del modello tramite criterio AIC
# ==============================================================================

library(tidyverse)
library(bmfaToolkits)
source("3_funzioni.R")

dati <- read_csv("dataset_finale_tesi_yj.csv")

variabili <- c("track_popularity", "age", "duration_ms",
               "danceability", "energy", "loudness", "valence",
               "tempo", "speechiness", "acousticness", "liveness",
               "instrumentalness", "luminescence")

#conversione del dataset in lista di matrici (una per playlist)
X_s <- split(dati %>% select(all_of(variabili)), dati$playlist_name)
X_s <- lapply(X_s, as.matrix)
X_s <- lapply(X_s, function(m) {dimnames(m) <- NULL; m})
playlists <- names(X_s)

results <- data.frame()

#tentativi (inserire in aic_cavi il numero di parametri del modello calcolati)
impl1 <- cavi_multistart(X_s, 13, 13, 1)

res_row <- aic_cavi(
  loglik = impl1$logLik,
  K = 4,
  J_s = c(3, 4, 3, 3, 3, 3),
  P = 13
)
res_row$K_start <- 13
res_row$J_start <- 13

results <- rbind(results, res_row)

impl2 <- cavi_multistart(X_s, 12, 12, 1)

res_row <- aic_cavi(
  loglik = impl2$logLik,
  K = 3,
  J_s = c(2, 3, 3, 2, 3, 3),
  P = 13
)
res_row$K_start <- 12
res_row$J_start <- 12

results <- rbind(results, res_row)

impl3 <- cavi_multistart(X_s, 11, 11, 1)

res_row <- aic_cavi(
  loglik = impl3$logLik,
  K = 4,
  J_s = c(3, 3, 3, 3, 3, 3),
  P = 13
)
res_row$K_start <- 11
res_row$J_start <- 11

results <- rbind(results, res_row)

impl4 <- cavi_multistart(X_s, 10, 10, 1)

res_row <- aic_cavi(
  loglik = impl4$logLik,
  K = 4,
  J_s = c(2, 3, 3, 2, 2, 2),
  P = 13
)
res_row$K_start <- 10
res_row$J_start <- 10

results <- rbind(results, res_row)

impl5 <- cavi_multistart(X_s, 9, 9, 1)

res_row <- aic_cavi(
  loglik = impl5$logLik,
  K = 5,
  J_s = c(3, 4, 3, 3, 4, 4),
  P = 13
)
res_row$K_start <- 9
res_row$J_start <- 9

results <- rbind(results, res_row)

impl6 <- cavi_multistart(X_s, 12, 8, 1)

res_row <- aic_cavi(
  loglik = impl6$logLik,
  K = 3,
  J_s =  c(2, 3, 3, 2, 3, 3),
  P = 13
)
res_row$K_start <- 12
res_row$J_start <- 8

results <- rbind(results, res_row)

impl7 <- cavi_multistart(X_s, 13, 10,  1)

res_row <- aic_cavi(
  loglik = impl7$logLik,
  K = 4,
  J_s = c(3, 4, 3, 3, 3, 3),
  P = 13
)
res_row$K_start <- 13
res_row$J_start <- 10

results <- rbind(results, res_row)

impl8 <- cavi_multistart(X_s, 11, 6, 1)

res_row <- aic_cavi(
  loglik = impl8$logLik,
  K = 4,
  J_s =  c(3, 3, 2, 2, 3, 3),
  P = 13
)
res_row$K_start <- 11
res_row$J_start <- 6

results <- rbind(results, res_row)

impl9 <- cavi_multistart(X_s, 10, 5, 1)

res_row <- aic_cavi(
  loglik = impl9$logLik,
  K = 4,
  J_s = c(2, 3, 3, 2, 2, 2),
  P = 13
)
res_row$K_start <- 10
res_row$J_start <- 5

results <- rbind(results, res_row)

impl10 <- cavi_multistart(X_s, 9, 5, 1)

res_row <- aic_cavi(
  loglik = impl10$logLik,
  K = 5,
  J_s = c(3, 4, 3, 3, 3, 4),
  P = 13
)
res_row$K_start <- 9
res_row$J_start <- 5

results <- rbind(results, res_row)
results <- results %>% arrange(AIC)