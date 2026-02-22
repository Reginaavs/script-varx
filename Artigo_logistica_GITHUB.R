# Regina Ávila and Beatriz Del Polzo (2026)
# Article: Assessment of the Impacts of Diesel Prices on Agricultural Freight Rates and Inflation in Brazil
# ============================================================
# ESTRATÉGIAS:
# A) VARIÁVEIS EM LOG E NA PRIMEIRA DIFERENÇA
# B) ANÁLISE DE QUEBRAS ESTRUTURAIS
# C) MODELOS VAR, VARX E VARX_BREAKS
# d) IRF ortogonalizada (Cholesky) no VARX -> UMA VEZ QUE AS ESTIMAÇÕES COM SVAR
# NÃO PASSARAM NO TESTE DE SOBREAJUSTE.
# ============================================================

library(vars)         # VAR, VARX, roots(), stability()
library(readxl)       # read_excel()
library(dplyr)        # filter(), mutate(), arrange(), left_join()
library(stringr)      # str_replace()
library(lubridate)    # quarter(), floor_date()
library(janitor)      # clean_names()
library(strucchange)  # breakpoints(), efp(), sctest()
library(forecast)   # autoplot(ts)
library(ggplot2)

options(scipen = 999)

# ============================================================
# 1) Leitura dos dados e pré-processamento
# ============================================================
path_xlsx <- "C:... .xlsx"

Dados_artigo <- read_excel(path_xlsx) %>%
  clean_names()

brent_fob <- read_excel(path_xlsx, sheet = "Planilha2")

# Garantir periodo como Date (evita POSIXct)
Dados_artigo <- Dados_artigo %>%
  mutate(periodo = as.Date(periodo))

Dados_artigo <- Dados_artigo %>%
  mutate(cambio = as.numeric(str_replace(as.character(cambio), ",", ".")))

# ============================================================
# 2) Brent FOB: média mensal e merge com base principal
# ============================================================
brent <- brent_fob %>%
  mutate(preco = as.numeric(str_replace(as.character(`brent(FOB)`), ",", "."))) %>%
  mutate(mes_ano = floor_date(as.Date(Data), "month")) %>%
  group_by(mes_ano) %>%
  summarise(brent_avg = mean(preco, na.rm = TRUE), .groups = "drop") %>%
  arrange(mes_ano)

Dados_artigo <- Dados_artigo %>%
  left_join(brent, by = c("periodo" = "mes_ano"))

str(Dados_artigo)

# ============================================================
# 3) Recorte 2017–2024
# ============================================================
df <- Dados_artigo %>%
  filter(periodo >= as.Date("2017-01-01"),
         periodo <= as.Date("2024-12-01")) %>%
  arrange(periodo)

# ============================================================
# Plot das variáveis para observar comportamento
# ============================================================
diesel_ts <- ts(df$diesel, start = c(2017,1), frequency = 12)
frete_ts  <- ts(df$frete,  start = c(2017,1), frequency = 12)
ipca_ts   <- ts(df$indice_ipca_2013, start = c(2017,1), frequency = 12)

# plots individuais (base R)
plot(diesel_ts, xlab = "Tempo", ylab = "Preço do Diesel (R$/l)")
plot(frete_ts,  xlab = "Tempo", ylab = "Frete (R$/t x 1000km)")
plot(ipca_ts,   xlab = "Tempo", ylab = "Índice IPCA (base 2013=100)")

# figura única com facets (diesel, frete e variação do IPCA)
ts_data <- ts(cbind(diesel_ts, frete_ts, vipca_ts),
              start = c(2017, 1), frequency = 12)

autoplot(ts_data, facets = TRUE) +
  ggtitle("Séries Temporais (2017–2024): Diesel, Frete e IPCA Amplo")

#Testando estacionariedade

ldiesel <- log(df$diesel)
lfrete  <- log(df$frete)
lipca_i <- log(df$indice_ipca_2013)

summary(ur.df(ldiesel, type="trend", lags=3))
summary(ur.df(lfrete,  type="trend", lags=3))
summary(ur.df(lipca_i, type="trend", lags=3))

kpss.test(ldiesel, null="Level")
kpss.test(lfrete,  null="Level")
kpss.test(lipca_i, null="Trend")

dldiesel <- diff(ldiesel)
dlfrete  <- diff(lfrete)
dlipca   <- diff(lipca_i)

kpss.test(dldiesel, null="Level")
kpss.test(dlfrete,  null="Level")
kpss.test(dlipca,   null="Level")
# ============================================================
# 4) Dummies trimestrais (sazonalidade)
# ============================================================
df <- df %>%
  mutate(trimestre = factor(quarter(periodo)))

df$trimestre <- relevel(df$trimestre, ref = "4")

trim_mat  <- model.matrix(~ trimestre, data = df)
trim_exog <- trim_mat[, -1, drop = FALSE]
colnames(trim_exog) <- c("trimestre1", "trimestre2", "trimestre3")

# ============================================================
# 5) Logs e diferenças (crescimento mensal)
# ============================================================
ldiesel <- log(df$diesel)
lfrete  <- log(df$frete)
lipca   <- log(df$indice_ipca_2013)
lcambio <- log(df$cambio)
lbrent  <- log(df$brent_avg)

dldiesel <- diff(ldiesel)
dlfrete  <- diff(lfrete)
dlipca   <- diff(lipca)
dlcambio <- diff(lcambio)
dlbrent  <- diff(lbrent)

# Datas alinhadas com as diferenças (perde 1º mês)
periodo_d <- df$periodo[-1]

# Alinhar dummies trimestrais com as diferenças
trim_exog_d <- trim_exog[-1, , drop = FALSE]

# ============================================================
# 6) Quebras estruturais nos NÍVEIS (breakpoints) — 
# ============================================================
dfb_diesel <- df %>%
  mutate(ldiesel = log(diesel)) %>%
  mutate(dldiesel = c(NA, diff(ldiesel))) %>%
  filter(!is.na(dldiesel)) %>%
  select(periodo, dldiesel)

bp_diesel <- breakpoints(dldiesel ~ periodo, data = dfb_diesel)
summary(bp_diesel)

nb_diesel <- which.min(BIC(bp_diesel)) - 1
bp_dates_diesel <- dfb_diesel$periodo[bp_diesel$breakpoints[1:nb_diesel]]
bp_dates_diesel

# Plot série em nível (ldiesel) + tendência segmentada
par(mar = c(4,4,2,1))
plot(dfb_diesel$periodo, dfb_diesel$dldiesel, type = "l",
     col = "black", lwd = 1.2,
     xlab = "Período", ylab = "log(Diesel)", xaxt = "n")
axis.Date(1,
          at = seq(min(dfb_diesel$periodo), max(dfb_diesel$periodo), by = "2 years"),
          format = "%Y")
lines(dfb_diesel$periodo, fitted(bp_diesel, breaks = nb_diesel),
      col = "red", lwd = 2)
abline(v = bp_dates_diesel, col = "blue", lty = 2, lwd = 2)
legend("topleft",
       legend = c("Série", "Tendência segmentada", "Quebra estrutural"),
       col    = c("black","red","blue"),
       lty    = c(1,1,2),
       lwd    = c(1,2,2),
       bty    = "n",
       cex    = 0.9)

# FRETE (dlog) 
dfb_frete <- df %>%
  mutate(lfrete  = log(frete),
         dlfrete = lfrete - lag(lfrete)) %>%
  filter(!is.na(dlfrete)) %>%
  mutate(t = row_number()) %>%
  select(periodo, t, dlfrete)

bp_frete <- breakpoints(dlfrete ~ t, data = dfb_frete)
summary(bp_frete)

nb_frete <- which.min(BIC(bp_frete)) - 1

bp_dates_frete <- if (nb_frete > 0) dfb_frete$periodo[bp_frete$breakpoints[1:nb_frete]] else as.Date(character(0))
bp_dates_frete


#IPCA
dfb_ipca <- df %>%
  mutate(lipca  = log(indice_ipca_2013),
         dlipca = lipca - lag(lipca)) %>%
  filter(!is.na(dlipca)) %>%
  mutate(t = row_number()) %>%
  select(periodo, t, dlipca)

# 2) Breakpoints (usar t; evita problemas com Date)
bp_ipca <- suppressWarnings(breakpoints(dlipca ~ t, data = dfb_ipca))
summary(bp_ipca)

#  1) extrair BIC do summary (robusto mesmo quando $BIC é NULL) ----
out <- capture.output(summary(bp_ipca))

bic_line <- out[grepl("^BIC\\s+", out)]   # linha que começa com "BIC"
bic_line

bic_vals <- as.numeric(strsplit(trimws(sub("^BIC\\s+", "", bic_line)), "\\s+")[[1]])
names(bic_vals) <- 0:(length(bic_vals) - 1)  # m = 0..5
bic_vals

# 2) nº ótimo de quebras pelo BIC ----
nb_ipca <- which.min(bic_vals) - 1
nb_ipca

# pegar os ÍNDICES (vetor) das quebras para nb_ipca
bp_idx_ipca <- breakpoints(bp_ipca, breaks = nb_ipca)$breakpoints
bp_idx_ipca

# converter para DATAS
bp_dates_ipca <- dfb_ipca$periodo[bp_idx_ipca]
bp_dates_ipca

# 5) Plot: série (dlipca) + tendência segmentada + quebras
par(mar = c(4,4,2,1))
plot(dfb_ipca$periodo, dfb_ipca$dlipca, type = "l",
     col = "black", lwd = 1.2,
     xlab = "Período", ylab = "Δlog(IPCA)",
     main = "Quebras estruturais em Δlog(IPCA)",
     xaxt = "n")

axis.Date(1,
          at = seq(min(dfb_ipca$periodo), max(dfb_ipca$periodo), by = "2 years"),
          format = "%Y")

lines(dfb_ipca$periodo, fitted(bp_ipca, breaks = nb_ipca),
      col = "red", lwd = 2)

if (length(bp_dates_ipca) > 0) {
  abline(v = bp_dates_ipca, col = "blue", lty = 2, lwd = 2)
}

legend("topleft",
       legend = c("Série", "Tendência segmentada", "Quebra estrutural"),
       col    = c("black","red","blue"),
       lty    = c(1,1,2),
       lwd    = c(1,2,2),
       bty    = "n",
       cex    = 0.9)

par(mfrow = c(3,1), mar = c(4,4,2,1))

# ============================================================
# 7) Dummies de ruptura (STEP) para robustez no VARX
# ============================================================
D_diesel2020m05 <- as.integer(periodo_d >= as.Date("2020-05-01"))
D_ipca2020m08 <- as.integer(periodo_d >= as.Date("2020-08-01"))
D_ipca2022m04 <- as.integer(periodo_d >= as.Date("2022-04-01"))

# ============================================================
# 8) Montagem de Y e matrizes exógenas (X1 = baseline; X = com rupturas)
# ============================================================
Y <- cbind(dldiesel = dldiesel,
           dlfrete  = dlfrete,
           dlipca   = dlipca)

X1 <- cbind(dlcambio = dlcambio,
            dlbrent  = dlbrent,
            trim_exog_d)

X <- cbind(X1,
           D_ipca2020m8  = D_ipca2020m08,
           D_ipca2022m04 = D_ipca2022m04,
           D_diesel2020m05 = D_diesel2020m05 )


stopifnot(nrow(Y) == nrow(X1), nrow(Y) == nrow(X))

# ============================================================
# 9) Estimação: VAR, VARX baseline e VARX com rupturas
# ============================================================
# Y já está cbind(dldiesel, dlfrete, dlipca)
df <- as.data.frame(Y)
VARselect(df, lag.max = 12, type = "const")

p <- 3

var        <- VAR(Y, p = p, type = "const")                  # baseline
varx       <- VAR(Y, p = p, type = "const", exogen = X1)     # baseline
varx_break <- VAR(Y, p = p, type = "const", exogen = X)      # robustez

# ============================================================
# 10) Comparação de modelos: raízes, ICs e logLik
# ============================================================
# Estabilidade dinâmica (raízes < 1)
r_varx       <- roots(varx)
r_varx_break <- roots(varx_break)

print(r_varx);       print(all(Mod(r_varx) < 1))
print(r_varx_break); print(all(Mod(r_varx_break) < 1))

# Critérios de informação
cat("VAR:        AIC=", AIC(var),        " BIC=", BIC(var),        "\n")
cat("VARX:       AIC=", AIC(varx),       " BIC=", BIC(varx),       "\n")
cat("VARX_break: AIC=", AIC(varx_break), " BIC=", BIC(varx_break), "\n")

# LogLik
cat("VAR:        ", summary(var)$logLik,        "\n")
cat("VARX:       ", summary(varx)$logLik,       "\n")
cat("VARX_break: ", summary(varx_break)$logLik, "\n")

# Summaries
summary(var)
summary(varx)
summary(varx_break)

# ============================================================
# 11) DIAGNÓTICOS
# ============================================================
run_diagnostics <- function(model, name, lags_pt = 12, lags_arch = 12,
                            do_plots = TRUE, do_univariate_lb = TRUE) {
  cat("\n====================================================\n")
  cat("DIAGNÓSTICOS:", name, "\n")
  cat("====================================================\n")
  
  # Estabilidade paramétrica (CUSUM/MOSUM)
  stab_cusum <- stability(model, type = "OLS-CUSUM")
  stab_mosum <- stability(model, type = "OLS-MOSUM")
  
  if (do_plots) {
    plot(stab_cusum, main = paste0(name, " - OLS-CUSUM"))
    plot(stab_mosum, main = paste0(name, " - OLS-MOSUM"))
  }
  
  # Autocorrelação (Portmanteau ajustado)
  cat("\n--- Serial correlation (Portmanteau adjusted) ---\n")
  print(serial.test(model, lags.pt = lags_pt, type = "PT.adjusted"))
  
  # Heterocedasticidade (ARCH multivariado)
  cat("\n--- ARCH test (multivariate) ---\n")
  print(arch.test(model, lags.multi = lags_arch))
  
  # Normalidade (Jarque-Bera multivariado)
  cat("\n--- Normality test (multivariate JB) ---\n")
  print(normality.test(model))
  
  # (Opcional) Ljung-Box univariado por equação (resíduos)
  if (do_univariate_lb) {
    cat("\n--- Ljung-Box por equação (resíduos) ---\n")
    res <- residuals(model)
    for (j in seq_len(ncol(res))) {
      cat("\nEq:", colnames(res)[j], "\n")
      print(Box.test(res[, j], lag = lags_pt, type = "Ljung-Box"))
    }
  }
  
  invisible(list(
    stability_cusum = stab_cusum,
    stability_mosum = stab_mosum,
    serial_pt = serial.test(model, lags.pt = lags_pt, type = "PT.adjusted"),
    arch = arch.test(model, lags.multi = lags_arch),
    normality = normality.test(model)
  ))
}

# ----------------------------
# Rodar para os 3 modelos
# ----------------------------
diag_var        <- run_diagnostics(var,        "VAR",        lags_pt = 12, lags_arch = 12)
diag_varx       <- run_diagnostics(varx,       "VARX",       lags_pt = 12, lags_arch = 12)
diag_varx_break <- run_diagnostics(varx_break, "VARX_break", lags_pt = 12, lags_arch = 12)

# ============================================================
# 11) IRF ORTOGONALIZADA (CHOLESKY) NO VARX
# ============================================================
# Ordem: [diesel → frete → IPCA]
# ============================================================
stopifnot(all(colnames(Y) == c("dldiesel","dlfrete","dlipca")))
set.seed(123)

H    <- 24      # horizonte
RUNS <- 2000    # bootstrap
CI   <- 0.95

# ------------------------------------------------------------
# 1) Calcular IRFs (com bootstrap) + Plots individuais padrão
# ------------------------------------------------------------
irf_A_diesel_frete <- irf(
  varx, impulse="dldiesel", response="dlfrete",
  n.ahead=H, boot=TRUE, runs=RUNS, ci=CI, ortho=TRUE
)

irf_A_diesel_ipca <- irf(
  varx, impulse="dldiesel", response="dlipca",
  n.ahead=H, boot=TRUE, runs=RUNS, ci=CI, ortho=TRUE
)

irf_A_frete_ipca <- irf(
  varx, impulse="dlfrete", response="dlipca",
  n.ahead=H, boot=TRUE, runs=RUNS, ci=CI, ortho=TRUE
)

# Plots individuais (padrão do pacote vars)
plot(irf_A_diesel_frete)
plot(irf_A_diesel_ipca)
plot(irf_A_frete_ipca)

# (Opcional) FEVD ortogonal (compatível com Cholesky)
fevd_A <- fevd(varx, n.ahead = 12)
plot(fevd_A)

# ------------------------------------------------------------
# 2) Figura única: 3 IRFs empilhadas (com bandas)
# ------------------------------------------------------------
# Função auxiliar para plotar 1 IRF com bandas
plot_irf_bands <- function(irf_obj, impulse, response, main_title, H) {
  h <- 0:H
  
  resp  <- irf_obj$irf[[impulse]][, response]
  lower <- irf_obj$Lower[[impulse]][, response]
  upper <- irf_obj$Upper[[impulse]][, response]
  
  plot(h, resp, type="l", lwd=2,
       ylim=range(c(lower, upper)),
       xlab="Horizonte (meses)",
       ylab="Resposta",
       main=main_title)
  
  lines(h, lower, lty=2)
  lines(h, upper, lty=2)
  abline(h=0, lty=2)
}

# Layout vertical (uma única figura com 3 painéis)
op <- par(mfrow = c(3,1), mar = c(4,4,3,1))

plot_irf_bands(irf_A_diesel_frete, "dldiesel", "dlfrete",
               "Choque Diesel → Frete", H)

plot_irf_bands(irf_A_diesel_ipca, "dldiesel", "dlipca",
               "Choque Diesel → IPCA", H)

plot_irf_bands(irf_A_frete_ipca, "dlfrete", "dlipca",
               "Choque Frete → IPCA", H)

par(op)  # restaura parâmetros gráficos originais
