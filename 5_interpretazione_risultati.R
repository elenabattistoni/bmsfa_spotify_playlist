# ==============================================================================
# Stima del modello e interpretazione
# ==============================================================================

library(tidyverse)
library(reshape2)
library(bmfaToolkits)
library(patchwork)
library(gridExtra)
library(igraph)

source("3_funzioni.R")

palette_col <- colorRampPalette(c("#B2182B", "#F7F7F7", "#08519C"))
dati <- read_csv("dataset_finale_tesi_yj.csv")

variabili <- c("track_popularity", "age", "duration_ms", 
               "danceability", "energy", "loudness", "valence", 
               "tempo", "speechiness", "acousticness", "liveness", "instrumentalness", 
               "luminescence")

X_s <- split(dati %>% select(all_of(variabili)), dati$playlist_name)
X_s <- lapply(X_s, as.matrix)
X_s <- lapply(X_s, function(m) {dimnames(m) <- NULL; m})
playlists <- names(X_s)

##stima del modello
impl_cavi <- cavi_multistart(X_s, 12, 8, 1)

fit <- impl_cavi$modello

names(fit$LambdaList) <- playlists
Psi <- fit$PsiList

# post-processing fattori 
Phi <- fit$Phi %>% reorder_loading() %>% switch_sign()
rownames(Phi) <- variabili

Lambda1 <- fit$LambdaList$`’80s Hard Rock` %>% reorder_loading() %>% 
  switch_sign()
rownames(Lambda1) <- variabili
Lambda2 <- fit$LambdaList$`Big Room EDM` %>% reorder_loading() %>% 
  switch_sign()
rownames(Lambda2) <- variabili
Lambda3 <- fit$LambdaList$`Latin Pop Classics` %>% reorder_loading() %>% 
  switch_sign()
rownames(Lambda3) <- variabili
Lambda4 <- fit$LambdaList$`Pop - Pop UK - 2019 - Canadian Pop - 2019 - Pop` %>% 
  reorder_loading() %>% switch_sign()
rownames(Lambda4) <- variabili
Lambda5 <- fit$LambdaList$`The 1950s/1960s/1970s/1980s/1990s/2000s/2010s with pop/r&b/soul/boogie/dance/jazz/hip hop/hop/rap.` %>% 
  reorder_loading() %>% switch_sign()
rownames(Lambda5) <- variabili
Lambda6 <- fit$LambdaList$`Trap Nation` %>% reorder_loading() %>%
  switch_sign()
rownames(Lambda6) <- variabili


#matrici ricostruite dal modello stimato
SigmaPhi <- fit$SigmaPhi
SigmaLambda <- fit$SigmaLambdaList
SigmaMarginal <- fit$SigmaMarginal

rownames(SigmaPhi) <- variabili
colnames(SigmaPhi) <- variabili

for (i in 1:6){
  rownames(SigmaMarginal[[i]]) <- variabili
  colnames(SigmaMarginal[[i]]) <- variabili
  rownames(SigmaLambda[[i]]) <- variabili
  colnames(SigmaLambda[[i]]) <- variabili
}

                                              
##interpretazione

#scomposizione della varianza
var_decomp <- list()
for (i in 1:6) {
  tot <- diag(SigmaMarginal[[i]])
  com <- diag(SigmaPhi)
  spec <- diag(SigmaLambda[[i]])
  idiosinc <- Psi[[i]]         
  
  var_decomp[[i]] <- data.frame(
    p_comune = com/tot,
    p_specifico = spec/tot,
    p_idiosincratico = idiosinc/tot
  )
}
var_decomp

df_facet <- imap_dfr(var_decomp, function(df, idx) {
  df %>%
    mutate(variabile = variabili,
           playlist = playlists[idx])}) %>%
  pivot_longer(cols = c(p_comune, p_specifico, p_idiosincratico),
               names_to = "Componente",
               values_to = "Quota") %>%
  mutate(Componente = factor(Componente,
                             levels = c("p_idiosincratico", "p_specifico", "p_comune"),
                             labels = c("Idiosincratica", "Studio-specifica", "Comune")),
         variabile = factor(variabile, levels = rev(variabili)))

generi <- c("ROCK", "EDM", "LATIN", "POP", "R&B", "RAP")

df_facet <- df_facet %>%
  mutate(genere = factor(playlist, levels = unique(playlist), labels = generi))

p_facet <- ggplot(df_facet, aes(x = Quota, y = variabile, fill = Componente)) +
  geom_col(width = 0.7) +
  facet_wrap(~ genere, ncol = 2) +
  scale_fill_manual(values = c(
    "Comune"           = "#08519C",  
    "Studio-specifica" = "#6BAED6",  
    "Idiosincratica"   = "#EFF3FF"  
  )) +
  scale_x_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  labs(
    x = "Proporzione di varianza",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title   = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.y  = element_text(color = "black", size = 13),
    axis.text.x  = element_text(color = "black", size = 10)
  )

#visualizzazione dei loadings
#Phi
plotPhi <- bmfaToolkits::plot_single_loadings(Phi,
                                              y_label = variabili,
                                              fill_limits = c(-1, 1),
                                              show_colorbar = TRUE) +
  scale_fill_gradientn(colours = palette_col(100),
                       name = "Loadings",
                       values = scales::rescale(c(-1, 0, 1)),
                       limits = c(-1, 1),
                       guide = guide_colorbar(barheight = unit(10, "cm"),
                                              barwidth  = unit(1.2, "cm"),
                                              title.position = "top",
                                              title.hjust = 0.5)) + 
  theme(
    legend.position = "right",
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y  = element_text(color = "black", size = 15),
    axis.text.x  = element_text(color = "black", size = 15))

#Lambda_s (la legenda è mostrata solo su un pannello)
plotL1 <- bmfaToolkits::plot_single_loadings(Lambda1, 
                                             y_label = variabili, 
                                             fill_limits = c(-1, 1)) + 
  ggtitle("ROCK")
plotL2 <- bmfaToolkits::plot_single_loadings(Lambda2, 
                                             y_label = colnames(X_s[[1]]),
                                             fill_limits = c(-1, 1)) + 
  ggtitle("EDM")
plotL3 <- bmfaToolkits::plot_single_loadings(Lambda3, 
                                             y_label = variabili,
                                             fill_limits = c(-1, 1)) + 
  ggtitle("LATIN")
plotL4 <- bmfaToolkits::plot_single_loadings(Lambda4, 
                                             y_label = colnames(X_s[[1]]),
                                             fill_limits = c(-1, 1),
                                             show_colorbar = T) + 
  ggtitle("POP")
plotL5 <- bmfaToolkits::plot_single_loadings(Lambda5, 
                                             y_label = variabili,
                                             fill_limits = c(-1, 1)) + 
  ggtitle("R&B")
plotL6 <- bmfaToolkits::plot_single_loadings(Lambda6, 
                                             y_label = colnames(X_s[[1]]),
                                             fill_limits = c(-1, 1)) + 
  ggtitle("RAP")

plotLambda <- (plotL1 + plotL2) /
  (plotL3 + plotL4) / (plotL5 + plotL6) + plot_layout(guides = "collect") & 
  scale_fill_gradientn(colours = palette_col(100),
                       name = "Loadings",
                       values = scales::rescale(c(-1, 0, 1)),
                       limits = c(-1, 1),
                       guide = guide_colorbar(barheight = unit(22, "cm"),
                                              barwidth  = unit(1.8, "cm"),
                                              title.position = "top",
                                              title.hjust = 0.5)) & 
  theme(legend.position = "right",
        legend.text  = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"),
        plot.title   = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.text.y  = element_text(color = "black", size = 13),
        axis.text.x  = element_text(color = "black", size = 13))

#grafi delle relazioni tra variabili:
#sono grafi non orientati costruiti a partire dalle matrici di covarianza 
#ricostruite dai fattori comuni e studio-specifici
#le relazioni con valore minore della soglia non sono rappresentate
#gli archi blu rappresentano una relazione positiva, quelli rossi una negativa
grafo_sigma <- function(Sigma, threshold = 0.15, titolo = "") {
  imp_nodo <- diag(Sigma)
  mat <- Sigma
  diag(mat) <- 0
  mat[abs(mat) < threshold] <- 0
  
  imp_range = c(0.002, 0.77)
  w_range = c(threshold, 0.58)
  
  g <- graph_from_adjacency_matrix(mat, mode = "undirected", 
                                   weighted = T, diag = F)
  
  V(g)$importance <- imp_nodo[V(g)$name]
  g <- delete_vertices(g, degree(g) == 0)

  node_sizes <- 20 + 20 * (V(g)$importance - imp_range[1]) / diff(imp_range)
  
  edge_weights <- E(g)$weight
  edge_widths <- 0.5 + 2 * (abs(edge_weights) - w_range[1]) / diff(w_range)
  edge_colors <- ifelse(edge_weights > 0, "#08519C", "#B2182B")
  
  set.seed(123)
  plot(g,
       cex.main = 80,
       layout = layout_with_fr(g, weights = abs(edge_weights)),
       vertex.size = node_sizes,
       vertex.color = adjustcolor("darkgrey", alpha.f = 0.8),
       vertex.frame.color = "white",
       vertex.label = V(g)$name,
       vertex.label.color = "black",
       vertex.label.family = "sans", 
       vertex.label.cex = 1.5,
       vertex.label.pos = 1,
       edge.width = edge_widths,
       edge.color = edge_colors,
       edge.curved = 0.15)
  
  title(main = titolo, cex.main = 2.5, line = -1)
}

#grafo comune
grafo_sigma(SigmaPhi) 

#grafi specifici per playlist
par(mfrow = c(3, 2), mar = c(1, 1, 3, 1))
grafo_sigma(SigmaLambda[[1]], titolo = "ROCK") 
grafo_sigma(SigmaLambda[[2]], titolo = "EDM") 
grafo_sigma(SigmaLambda[[3]], titolo = "LATIN") 
grafo_sigma(SigmaLambda[[4]], titolo = "POP") 
grafo_sigma(SigmaLambda[[5]], titolo = "R&B") 
grafo_sigma(SigmaLambda[[6]], titolo = "RAP") 
par(mfrow = c(1, 1))


#heatmap delle matrici di covarianza ricostruite
#matrice ricostruita dai fattori comuni
breaks <- seq(-1, 1, length.out = 201)

pheatmap::pheatmap(SigmaPhi,
                   cluster_rows = FALSE,
                   cluster_cols = FALSE,
                   color = palette_col(200),
                   breaks = breaks)

#matrici di covarianza ricostruite
generi <- c("ROCK", "EDM", "LATIN", "POP", "R&B", "RAP")
heatmaps <- list(NA, NA, NA, NA, NA, NA)
for (i in 1:6){
  heatmaps[[i]] <- pheatmap::pheatmap(SigmaMarginal[[i]], 
                                      cluster_rows = F, 
                                      cluster_cols = F, 
                                      color = palette_col(200), 
                                      main = generi[i],
                                      breaks = breaks,
                                      silent = T)
}
grid.arrange(grobs = lapply(heatmaps, function(x) x$gtable), ncol = 2)

generi <- c("ROCK", "EDM", "LATIN", "POP", "R&B", "RAP")
plots <- vector("list", length(generi))
for (i in 1:6){
  df <- melt(SigmaMarginal[[i]])
  colnames(df) <- c("Row", "Col", "Value")
  df$Row <- factor(df$Row, levels = rownames(SigmaMarginal[[i]]))
  df$Col <- factor(df$Col, levels = colnames(SigmaMarginal[[i]]))
  
  plots[[i]] <- ggplot(df, aes(x = Col, y = Row)) +
    geom_point(aes(size = abs(Value), color = Value)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev) +
    scale_color_gradient2(low      = "#B2182B",
                          mid      = "#F7F7F7",
                          high     = "#08519C",
                          midpoint = 0,
                          limits = c(-1, 1.4)) +
    scale_size_continuous(range = c(1, 8), guide="none") +
    labs(title = generi[i], x = "", y = "", color = "Valore", size = "|Valore|") +
    theme_bw() +
    theme(plot.title      = element_text(hjust = 0.5, face = "bold", size = 13),
          axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5, color = "black", size = 10),
          axis.text.y     = element_text(size = 10, color = "black"),
          panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
          legend.position = "right")}
grid.arrange(grobs = plots, ncol = 2)