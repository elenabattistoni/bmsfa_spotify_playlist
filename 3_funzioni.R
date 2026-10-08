# ==============================================================================
# Funzioni create
# ==============================================================================

library(tidyverse)
library(bmfaToolkits)

compute_loglik <- function(fit, X_s){
  ll_tot <- 0
  
  for (s in seq_along(X_s)){
    Sigma_s <- fit$SigmaMarginal[[s]]
    X_s_singola <- as.matrix(X_s[[s]])
    p <- ncol(X_s_singola)
    
    ll_s <- sum(mvtnorm::dmvnorm(X_s_singola, 
                                 mean = rep(0, p), sigma = Sigma_s, 
                                 log = T))
    ll_tot <- ll_tot + ll_s
  }
  return(ll_tot)
}


cavi_multistart <- function(X_s, K, J_s, n_seeds = 100){
  loglik_top <- -Inf
  mod_top <- NULL
  seed_top <- NA
  loglik_runs <- numeric(n_seeds)
  
  for (i in 1:n_seeds){
    set.seed(i)
    fit_CAVI <- fit_cavi_2step(Y_list = X_s,
                               k = K,
                               j_s = J_s,
                               scaling = F,
                               centering = F)
    loglik <- compute_loglik(fit_CAVI, X_s)
    loglik_runs[i] <- loglik
    if (loglik > loglik_top){
      loglik_top <- loglik
      mod_top <- fit_CAVI
      seed_top <- i
    }
  }
  return(list(modello = mod_top,
              logLik = loglik_top,
              seed = seed_top,
              loglik_runs = data.frame(seeds = 1:n_seeds,
                                       loglik = loglik_runs)))
}

aic_cavi <- function(loglik, K, J_s, P) {
  num_params <- P * K + P * sum(J_s) + P * 6 - (K*(K-1)/2) - sum((J_s*(J_s-1))/2)             
  aic <- 2 * num_params - 2 * loglik
  
  return(data.frame(
    K = K,
    J_s = paste(J_s, collapse = ","),
    num_params = num_params,
    loglik = loglik,
    AIC = aic
  ))
}
