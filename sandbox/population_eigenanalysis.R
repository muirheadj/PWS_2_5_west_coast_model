library(popbio)
library(DEoptim)

options(scipen = -4)

gamma_i <- function(sigma_i, lam, T_i){
  res <- ((sigma_i / lam) ^ T_i - (sigma_i / lam) ^ (T_i - 1)) /
    ((sigma_i / lam) ^ T_i - 1)
}


P_i <- function(p, T_i){
  p * (1 - p ^ (T_i - 1) ) / (1 - p ^ T_i)
}

G_i <- function(p, T_i) {
  p ^ T_i * (1 - p) / (1 - p ^ T_i)
}


balanus_lh <- list("larva" = list("sigma" = 0.515, "T" = 44),
                   "cyprid" = list("sigma" = 0.94, "T" = 12),
                   "juvenile" = list("sigma" = 0.58, "T" = 84),
                   "adult" = list("sigma" = 0.998, "T" = 2192))

balanus_fit <- function(log_gamma_3) {
  lam <- 1.0006

  sigma_1 <- balanus_lh[["larva"]][["sigma"]]
  gamma_1 <- 1/balanus_lh[["larva"]][["T"]]
  P_1 <- sigma_1 * (1 - gamma_1)
  G_1 <- sigma_1 * gamma_1

  sigma_2 <- balanus_lh[["cyprid"]][["sigma"]]
  gamma_2 <- gamma_i(sigma_i = sigma_2, lam = lam,
    T_i = balanus_lh[["cyprid"]][["T"]])

  P_2 <- sigma_2 * (1 - gamma_2)
  G_2 <- sigma_2 * gamma_2

  sigma_3 <- balanus_lh[["juvenile"]][["sigma"]]

  gamma_3 <- exp(log_gamma_3)
  P_3 <- sigma_3 * (1 - gamma_3)
  G_3 <- sigma_3 * gamma_3

  sigma_4 <- balanus_lh[["adult"]][["sigma"]]

  gamma_4 <- gamma_i(sigma_i = sigma_4, lam = lam,
  	T_i =  balanus_lh[["adult"]][["T"]])

  P_4 <- sigma_4 * (1 - gamma_4)

  F_4 <- 13.69 # 10000 eggs per season

  A <- matrix(c(P_1, 0, 0, F_4,
                G_1, P_2, 0, 0,
                0, G_2, P_3, 0,
                0, 0, G_3, P_4), nrow = 4, byrow = TRUE)
  gammas <- c("gamma1" = gamma_1, "gamma2" = gamma_2, "gamma3" = gamma_3,
    "gamma4" = gamma_4)
  res = list(A = A, gammas = gammas)
}


gamma_min_function <- function(gamma) {
  A_local <- balanus_fit(gamma)[["A"]]
  A_local[is.nan(A_local)] <- .Machine$double.eps

  res <- (1.0006 - eigen.analysis(A_local)$lambda1)^2
}


res <- DEoptim(gamma_min_function, lower = -7, upper = 0,
             control = list(trace = 1))

(A_new <- balanus_fit(res$optim$bestmem)[["A"]])

gammas <- balanus_fit(res$optim$bestmem)[["gammas"]]

(A_eigen <- eigen.analysis(A_new))
(fundamental.matrix(A_new))




(n_init <- ceiling(A_eigen$stable.stage * 1e10))
