# Strutture latenti nella musica: analisi fattoriale multi-studio bayesiana su playlist Spotify

Codice R della tesi triennale *"Strutture latenti nella musica: un'applicazione dell'analisi fattoriale multi-studio bayesiana su playlist Spotify"* (Università degli Studi di Padova, Dipartimento di Scienze Statistiche, Corso di Laurea in Statistica per l'Economia e l'Impresa, A.A. 2025/2026).

> **Autrice:** Elena Battistoni

> **Relatore:** Dott. Francesco Denti

Il lavoro applica la **Bayesian Multi-Study Factor Analysis**, stimata con **algoritmo CAVI** (Hansen et al., 2024), a sei playlist Spotify, una per genere considerato (EDM, Latin, Pop, R&B, Rap, Rock). Ogni playlist è trattata come uno "studio": il modello separa la struttura latente **comune** a tutti i generi da quella **specifica** di ciascuno.

------------------------------------------------------------------------

## Struttura del repository

```         
.
├── 1_pre_processing.R            # pulizia, modifiche alle variabili, scelta delle playlist, trasformazioni
├── 2_analisi_esplorativa.R       # boxplot prima/dopo la trasformazione e correlogrammi
├── 3_funzioni.R                  # funzioni di supporto (log-verosimiglianza, multistart CAVI, AIC)
├── 4_selezione_AIC.R             # confronto tra le dieci configurazioni (K_start, J_start) tramite AIC
└── 5_interpretazione_risultati.R # modello finale, decomposizione della varianza, loadings, grafi, heatmap
```

## Dati

Il dataset di partenza è **"30000 Spotify Songs"** (Joe Beach Capital, 2020), disponibile su [Kaggle](https://www.kaggle.com/datasets/joebeachcapital/30000-spotify-songs): 32833 brani di 6 generi, ottenuti tramite la Spotify API. **Non è incluso nel repository**: è stato salvato come `spotify_songs.csv`.

Le 13 variabili usate nell'analisi:

| Variabile | Descrizione |
|------------------------------------|------------------------------------|
| `track_popularity`, `duration_ms` | popolarità e durata del brano |
| `danceability`, `energy`, `loudness`, `valence`, `tempo`, `speechiness`, `acousticness`, `liveness`, `instrumentalness` | feature audio Spotify |
| `age` | età del brano: `2026 − anno di uscita dell'album` |
| `luminescence` | `key` e `mode` combinate in una scala lineare 1–24 costruita a partire dalla ruota di Camelot |

### Selezione del campione

Dopo l'eliminazione dei valori mancanti e dei brani duplicati nella stessa playlist, si considerano le playlist con esattamente 100 brani. Per ogni genere ne viene estratta una a caso. Le playlist ottenute nella tesi sono:

| Genere | Playlist |
|------------------------------------|------------------------------------|
| EDM | Big Room EDM |
| Latin | Latin Pop Classics |
| Pop | Pop - Pop UK - 2019 - Canadian Pop - 2019 - Pop |
| R&B | The 1950s/1960s/1970s/1980s/1990s/2000s/2010s with pop/r&b/soul/boogie/dance/jazz/hip hop/hop/rap. |
| Rap | Trap Nation |
| Rock | ’80s Hard Rock |

### Trasformazione

All'interno di ogni playlist le variabili sono trasformate con **Yeo-Johnson** (per correggere l'asimmetria, in vista dell'assunzione di normalità del modello) e poi **standardizzate** (media 0, varianza 1).

Nota: `speechiness`, `acousticness`, `liveness` e `instrumentalness` restano asimmetriche anche dopo la trasformazione e violano le assunzioni del modello; sono state mantenute per la loro rilevanza musicale.

## Il modello e la procedura di stima

- **Stima:** `fit_cavi_2step` (pacchetto `bmfaToolkits`) stima prima il modello con numeri di fattori iniziali sovradimensionati (`K_start`, `J_start`), tiene i fattori che spiegano più del 5% della varianza e ristima il modello con i soli `K*` e `J_s*` ottenuti. Internamente richiama `cavi_msfa` del pacchetto `VIMSFA`, che controlla la convergenza sulla variazione dei parametri e non sull'ELBO.
- **Log-verosimiglianza:** poiché il pacchetto non restituisce l'ELBO, il confronto tra stime usa la log-verosimiglianza marginale `Σ_s Σ_i log N(x_is ; 0, Σ̂_s)`.
- **Seed:** `cavi_multistart()` permette di ripetere la stima con più seed. A parità di `K_start` e `J_start`, seed diversi hanno dato la stessa log-verosimiglianza; per questo ogni configurazione è stimata una sola volta (`n_seeds = 1`, seed 1).
- **Selezione:** `aic_cavi()` calcola `AIC = 2k − 2·logLik`, con `k = P·K + P·Σ_s J_s + P·S − K(K−1)/2 − Σ_s J_s(J_s−1)/2`. Si tratta di un'estensione della formula per la FA classica: un'approssimazione ragionevole ma non formalmente verificata.
- **Modello finale** (`K_start = 12`, `J_start = 8`): 3 fattori comuni e `J_s = (2, 3, 3, 2, 3, 3)` fattori specifici.
- **Post-processing:** colonne dei loadings ordinate per varianza spiegata, segni invertiti in modo che la maggior parte dei valori di ogni colonna sia positiva.

## Riproducibilità e limiti noti

- Il preprocessing è un'unica pipeline standardizzata usata sia per l'analisi esplorativa sia per la stima; piccole differenze numeriche rispetto a quanto riportato in tesi sono possibili.
- La funzione di stima presenta un bug nel caso di un solo fattore studio-specifico (`J_s = 1`). Non si manifesta con le configurazioni usate qui.

## Riferimenti

- Bhattacharya, A. & Dunson, D. B. (2011). Sparse Bayesian infinite factor models. *Biometrika*, 98(2), 291–306.
- De Vito, R., Bellio, R., Trippa, L. & Parmigiani, G. (2019). Multi-study factor analysis. *Biometrics*, 75(1), 337–346.
- De Vito, R., Bellio, R., Trippa, L. & Parmigiani, G. (2021). Bayesian multistudy factor analysis for high-throughput biological data. *The Annals of Applied Statistics*, 15(4), 1723–1741.
- Hansen, B. (2024). *VIMSFA: Variational Inference for Bayesian Multi-Study Factor Analysis*. <https://github.com/blhansen/VIMSFA>
- Hansen, B. et al. (2024). Fast Variational Inference for Bayesian Factor Analysis in Single and Multi-Study Settings. arXiv:2305.13188.
- Liang, M. (2026). *bmfaToolkits*. <https://github.com/Mavis-Liang/bmfaToolkits>
- Liang, M. et al. (2026). A Tutorial on Bayesian Multi-Study Factor Analysis With Applications in Nutrition and Genomics. *Statistics in Medicine*, 45(10–12), e70531.
- Yeo, I.-K. & Johnson, R. A. (2000). A new family of power transformations to improve normality or symmetry. *Biometrika*, 87(4), 954–959.
