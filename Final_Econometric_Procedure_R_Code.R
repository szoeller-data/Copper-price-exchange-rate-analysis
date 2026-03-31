
#Econometric Framework : Tests and Estimations
# VARX(PERU): Δlog(REER), Δlog(Reserves)  ~  Δlog(Copper), IGREA
# Data: 4 Excel sheets (quarterly) in /mnt/data/
# - Copper, REER, Reserves: log-differences
# - IGREA: levels (can be negative)
# Includes: plots, ADF tests, lag selection (AIC/BIC), VARX, diagnostics,
#           dynamic multipliers for exogenous shocks, FEVD, fitted vs actual
############################################################


install.packages <- c("readxl", "zoo", "dplyr", "ggplot2", "vars", "urca", "tseries", "tidyr", depedencie = TRUE)
library(readxl)
library(zoo)
library(dplyr)
library(ggplot2)
library(vars)
library(urca)
library(tseries)
library(tidyr)

reer <- read_excel("Quarterly_REER_Average.xlsx")
reserves <- read_excel("Quarterly_Reserves(Stock)_2002_2024.xlsx")
copper <- read_excel("Quarterly_PCOPPUSDM(Copper)_labeled.xlsx")
igrea <- read_excel("Quarterly_IGREA_2002_2024.xlsx")

colnames(igrea)    <- c("date","IGREA")
colnames(copper)   <- c("date","COP")
colnames(reer)     <- c("date","REER")
colnames(reserves) <- c("date","RES")

#Convert date to quarterly index

igrea$date    <- as.yearqtr(igrea$date)
copper$date   <- as.yearqtr(copper$date)
reer$date     <- as.yearqtr(reer$date)
reserves$date <- as.yearqtr(reserves$date)

# Merge into one dataframe
df <- merge(reer, reserves, by="date")
df <- merge(df, copper, by="date")  
df <- merge(df, igrea, by="date")
df <- df[order(df$date), ]

# Logs (levels)
df$lREER <- log(df$REER)
df$lRES  <- log(df$RES)
df$lCOP  <- log(df$COP) 


# Make sure df$date_d exists as a Date
df$date_d <- as.Date(df$date, frac = 1)

# Create a long-form data frame
levels_long <- tidyr::pivot_longer(
  df[, c("date_d", "lCOP", "lREER", "lRES", "IGREA")],
  cols = -date_d,
  names_to = "series",
  values_to = "value"
)

# Recode series names
levels_long$series <- factor(
  levels_long$series,
  levels = c("lCOP", "lREER", "lRES", "IGREA"),
  labels = c("Log(COP)", "Log(REER)", "Log(RESERVES)", "IGREA")
)

# Plot
ggplot(levels_long, aes(x = date_d, y = value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~series, scales = "free_y", ncol = 2) +
  scale_x_date(
    breaks = as.Date(c(
      "2002-01-01",
      "2008-01-01",
      "2014-01-01",
      "2020-01-01",
      "2024-01-01"
    )),
    date_labels = "%Y"
  ) +
  labs(
    title = "Time-Series Multi-Plot in Levels (2002 Q1 – 2024 Q4)",
    subtitle = "Logs of REER, Reserves, Copper; IGREA in levels",
    x = "Year",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30"),
    axis.text.x = element_text(size = 10)
  )

names(df)

#long-form data frame
trans_long <- tidyr::pivot_longer(
  df[, c("date_d", "dCOP", "dREER", "dRES", "IGREA")],
  cols = -date_d,
  names_to = "series",
  values_to = "value"
)



print(colnames(df))
head(df)


# ---- Ensure differenced variables exist ----
df$dREER <- c(NA, diff(df$lREER))
df$dRES  <- c(NA, diff(df$lRES))
df$dCOP  <- c(NA, diff(df$lCOP))
df$dIGREA <- c(NA, diff(df$IGREA))
# Remove the first NA row
df <- df[-1, ]

# Make sure df$date_d is a Date
df$date_d <- as.Date(df$date, frac = 1)

# Pivot to long without select()
trans_long <- tidyr::pivot_longer(
  df[, c("date_d","dCOP","dREER","dRES","dIGREA")],
  cols = -date_d,
  names_to = "series",
  values_to = "value"
)

# Recode the panel names exactly as you want
trans_long$series <- factor(
  trans_long$series,
  levels = c("dCOP","dREER","dRES","dIGREA"),
  labels = c("ΔLog(COP)", "ΔLog(REER)", "ΔLog(RESERVES)", "ΔIGREA")
)

# Plot
ggplot(trans_long, aes(x = date_d, y = value)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.3) +
  geom_line(color = "darkgreen") +
  scale_x_date(
    breaks = as.Date(c(
      "2002-01-01",
      "2008-01-01",
      "2014-01-01",
      "2020-01-01",
      "2024-01-01"
    )),
    date_labels = "%Y"
  ) +
  labs(
    title = "Time-Series Multi-Plot in Stationary Transformations (2002 Q1 – 2024 Q4)",
    x = "Year",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 10)
  )


# Ensure differenced variables exist
df$dREER <- c(NA, diff(df$lREER))
df$dRES  <- c(NA, diff(df$lRES))
df$dCOP  <- c(NA, diff(df$lCOP))

df <- df[-1, ]   # remove first NA row
df$date_d <- as.Date(df$date, frac = 1)

# ---- 1) Plot: ΔLog(COP) ----
p1 <- ggplot(df, aes(x = date_d, y = dCOP)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray40") +
  geom_line(color="blue") +
  scale_x_date(
    breaks = as.Date(c(
      "2002-03-30",
      "2008-06-30",
      "2014-06-30",
      "2020-06-30",
      "2024-06-30"
    )),
    date_labels = "%Y"
  ) +
  labs(title = "ΔLog(COP)", x = "Year", y = NULL) +
  theme_minimal()


# ---- 2) Plot: ΔLog(REER) ----
p2 <- ggplot(df, aes(x = date_d, y = dREER)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray40") +
  geom_line(color="red") +
  scale_x_date(
    breaks = as.Date(c(
      "2002-03-30",
      "2008-06-30",
      "2014-06-30",
      "2020-06-30",
      "2024-06-30"
    )),
    date_labels = "%Y"
  ) +
  labs(title = "ΔLog(REER)", x = "Year", y = NULL) +
  theme_minimal()


# ---- 3) Plot: ΔLog(RESERVES) ----
p3 <- ggplot(df, aes(x = date_d, y = dRES)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray40") +
  geom_line(color="green") +
  scale_x_date(
    breaks = as.Date(c(
      "2002-03-30",
      "2008-06-30",
      "2014-06-30",
      "2020-06-30",
      "2024-06-30"
    )),
    date_labels = "%Y"
  ) +
  labs(title = "ΔLog(RESERVES)", x = "Year", y = NULL) +
  theme_minimal()


# ---- 4) Plot: IGREA ----
p4 <- ggplot(df, aes(x = date_d, y = dIGREA)) +
  geom_hline(yintercept = 0, linetype="dashed", color="gray40") +
  geom_line(color="purple") +
  scale_x_date(
    breaks = as.Date(c(
      "2002-03-30",
      "2008-06-30",
      "2014-06-30",
      "2020-06-30",
      "2024-06-30"
    )),
    date_labels = "%Y"
  ) +
  labs(title = "ΔIGREA", x = "Year", y = NULL) +
  theme_minimal()


# ---- Arrange grid ----
library(gridExtra)
library(grid)

main_title <- textGrob(
  "Quarterly Growth Rates and ΔIGREA Series (2002–2024)",
  gp = gpar(fontsize = 16, fontface = "bold")
)

grid.arrange(
  p1, p2, p3, p4,
  nrow = 2,
  top = main_title
)


    ######## STATIONARITY TESTS(ADF)#####

       print(adf.test(df$dREER))
        print(adf.test(df$dRES))
         print(adf.test(df$dCOP))
          print(adf.test(df$dIGREA))
     
          
          # list of series to test
          series_list <- list(
            "ΔLog(COP)"      = df$dCOP,
            "ΔLog(REER)"     = df$dREER,
            "ΔLog(RESERVES)" = df$dRES,
            "ΔIGREA"         = df$dIGREA
          )
          
          # ADF testing + classification function
          adf_summary <- function(x) {
            x <- na.omit(x)   # IMPORTANT
            
            test <- tseries::adf.test(x)
            
            stat <- as.numeric(test$statistic)
            pval <- test$p.value
            lag  <- test$parameter   # correct extraction
            
            order <- ifelse(pval < 0.05, "I(0)", "I(1)")
            
            data.frame(
              Test_Statistic = round(stat, 4),
              P_Value        = ifelse(pval < 0.001, "< 0.001", round(pval, 3)),
              Lag_Order      = lag,
              Integration    = order,
              stringsAsFactors = FALSE
            )
          }
          
          # run tests 
          results_adf <- do.call(rbind, lapply(series_list, adf_summary))
          results_adf# list of series to test

          #### CRISIS DUMMIES (EXOGENOUS CONTROLS) ####
          
          # Global Financial Crisis: 2008Q3–2009Q4
          df$GFC <- ifelse(df$date >= as.yearqtr("2008 Q3") &
                             df$date <= as.yearqtr("2009 Q4"), 1, 0)
          
          # COVID-19 crisis: 2020Q1–2021Q4
          df$COVID <- ifelse(df$date >= as.yearqtr("2020 Q1") &
                               df$date <= as.yearqtr("2021 Q4"), 1, 0)
          
            #### VARX ESTIMATION SET UP ########
          # Endogenous: [dREER, dRES]
          # Exogenous:  [dCOP, dIGREA, GFC, COVID]
          
           Y <- df[, c("dREER", "dRES")]
              
           X <- df[, c("dCOP", "dIGREA", "GFC", "COVID")]
          
              head(Y)
              head(X)
              
              df[df$GFC == 1, ]
              df[df$COVID == 1, ]
              
              ###########VARX Model constant + dummies Test #########
              
              
              lag_select <- VARselect(Y, lag.max=8, type="const", exogen= X)
              print(lag_select$criteria)
              
              
              ###“The lag length of the VARX model
              ##was selected using standard information criteria. 
              ###All criteria (AIC, HQ, SC, and FPE) unanimously selected one lag, 
              ##and the model was therefore estimated with a single lag.######
              
              
              ##VARX model##
              ###With a costant, the stability problem is resolve and unit circle responds as expected"
              
              varx_model_final <- VAR(Y, p = 1, type = "const", exogen = X)  
              summary(varx_model_final)
                  ###The stability of the VARX(1) specification without a deterministic constant was verified by inspecting the roots of the characteristic polynomial. All roots lie well inside the unit circle, indicating a dynamically stable system.###
                  
                  plot(roots(varx_model_final))
                  
                  ### VARX MODEL Diagnostics######
                  
                 
                  # 1 — Serial correlation
                  serial.test(varx_model_final, lags.pt = 12, type = "PT.asymptotic")
                  
                  # 2 — Normality
                  normality.test(varx_model_final)
                  
                  # 3 — Heteroscedasticity
                  arch.test(varx_model_final)
                  
                  ###Diagnostic tests confirm the absence of serial correlation and heteroscedasticity in the model residuals. The normality assumption is rejected, likely reflecting fat-tailed distributions common in macroeconomic time series. As inference is conducted via bootstrap methods, this does not affect the validity of the reported confidence intervals.
                  
                  ######## Dynamic Multipliers #############
                  
                  H <- 12
                  
                  sd_igrea <- sd(df$dIGREA, na.rm = TRUE)
                  sd_cop   <- sd(df$dCOP,   na.rm = TRUE)
                  
                  # baseline exogenous path (no shocks)
                  X_base <- matrix(0, nrow = H, ncol = ncol(X))
                  colnames(X_base) <- colnames(X)
                  
                  # IGREA shock (global activity)
                  X_igrea <- X_base
                  X_igrea[1, "dIGREA"] <- sd_igrea
                  
                  # Copper price shock
                  X_cop <- X_base
                  X_cop[1, "dCOP"] <- sd_cop
                  
                  # baseline exogenous path (no shocks)
X_base <- matrix(0, nrow = H, ncol = ncol(X))
colnames(X_base) <- colnames(X)

# IGREA shock (global activity)
X_igrea <- X_base
X_igrea[1, "dIGREA"] <- sd_igrea

# Copper price shock
X_cop <- X_base
X_cop[1, "dCOP"] <- sd_cop

pred_base  <- predict(varx_model_final, n.ahead = H, dumvar = X_base)
pred_igrea <- predict(varx_model_final, n.ahead = H, dumvar = X_igrea)
pred_cop   <- predict(varx_model_final, n.ahead = H, dumvar = X_cop)


dm_REER_igrea <- pred_igrea$fcst$dREER[, "fcst"] -
  pred_base$fcst$dREER[, "fcst"]

dm_RES_igrea  <- pred_igrea$fcst$dRES[, "fcst"] -
  pred_base$fcst$dRES[, "fcst"]


dm_REER_cop <- pred_cop$fcst$dREER[, "fcst"] -
  pred_base$fcst$dREER[, "fcst"]

dm_RES_cop  <- pred_cop$fcst$dRES[, "fcst"] -
  pred_base$fcst$dRES[, "fcst"]


###PLOT####

par(mfrow = c(2, 2))

# Copper → REER
plot(0:(H-1), dm_REER_cop, type = "l", lwd = 2,
     xlab = "Quarters",
     ylab = "Response of Δln(REER)",
     main = "Copper Price Shock → REER (Dutch Disease Channel)")
abline(h = 0, lty = 2)

# Copper → Reserves
plot(0:(H-1), dm_RES_cop, type = "l", lwd = 2,
     xlab = "Quarters",
     ylab = "Response of Δln(RES)",
     main = "Copper Price Shock → International Reserves")
abline(h = 0, lty = 2)

# IGREA → REER
plot(0:(H-1), dm_REER_igrea, type = "l", lwd = 2,
     xlab = "Quarters",
     ylab = "Response of Δln(REER)",
     main = "Global Activity Shock → REER")
abline(h = 0, lty = 2)

# IGREA → Reserves
plot(0:(H-1), dm_RES_igrea, type = "l", lwd = 2,
     xlab = "Quarters",
     ylab = "Response of Δln(RES)",
     main = "Global Activity Shock → International Reserves")
abline(h = 0, lty = 2)


#############Cumulative Effects #########

cumulative_table <- data.frame(
  shock = c("ΔIGREA (1 sd)", "ΔIGREA (1 sd)", "Δln(COP) (1 sd)", "Δln(COP) (1 sd)"),
  response = c("Δln(REER)", "Δln(RES)", "Δln(REER)", "Δln(RES)"),
  cumulative_log_change = c(
    sum(dm_REER_igrea),
    sum(dm_RES_igrea),
    sum(dm_REER_cop),
    sum(dm_RES_cop)
  )
)

cumulative_table$cumulative_percent_approx <-
  100 * cumulative_table$cumulative_log_change

print(cumulative_table)

# add only these two lines
cum_path_REER_cop <- cumsum(dm_REER_cop)
cum_path_RES_cop  <- cumsum(dm_RES_cop)

par(mfrow = c(1, 2))

# REER cumulative
plot(0:(H-1), 100 * cum_path_REER_cop, type = "l", lwd = 2,
     xlab = "Quarters",
     ylab = "Cumulative % change",
     main = "Cumulative Effect: Copper → REER",
     ylim = c(0, max(100 * cum_path_REER_cop) * 1.2))
abline(h = 0, lty = 2)

# Reserves cumulative
plot(0:(H-1), 100 * cum_path_RES_cop, type = "l", lwd = 2,
     xlab = "Quarters",
     ylab = "Cumulative % change",
     main = "Cumulative Effect: Copper → Reserves",
     ylim = c(0, max(100 * cum_path_RES_cop) * 1.2))
abline(h = 0, lty = 2)









########## BOOTSTRAPS ##############


library(vars)

Ymat <- as.matrix(Y)
Xmat <- as.matrix(X)

T <- nrow(Ymat)
k <- ncol(Ymat)

# residuals (aligned for VAR(1): length T-1)
Uhat <- as.matrix(residuals(varx_model_final))
stopifnot(nrow(Uhat) == T - 1)

# coefficient matrices
cf <- coef(varx_model_final)
endo <- colnames(Ymat)
exo  <- colnames(Xmat)

A1 <- matrix(0, k, k, dimnames = list(endo, endo))
Bx <- matrix(0, k, ncol(Xmat), dimnames = list(endo, exo))

for (eq in endo) {
  for (j in endo) {
    nm <- paste0(j, ".l1")
    if (nm %in% rownames(cf[[eq]])) A1[eq, j] <- cf[[eq]][nm, 1]
  }
  for (xj in exo) {
    if (xj %in% rownames(cf[[eq]])) Bx[eq, xj] <- cf[[eq]][xj, 1]
  }
}






simulate_varx1 <- function(A1, Bx, X, Ustar, y1) {
  T <- nrow(X)
  k <- ncol(A1)
  Ysim <- matrix(NA_real_, T, k)
  colnames(Ysim) <- rownames(A1)
  Ysim[1, ] <- y1
  
  for (t in 2:T) {
    Ysim[t, ] <- as.numeric(A1 %*% Ysim[t-1, ] +
                              Bx %*% X[t, ] +
                              Ustar[t-1, ])
  }
  Ysim
}



set.seed(123)
B <- 500

cum_REER_cop_boot <- cum_RES_cop_boot <-
  cum_REER_ig_boot  <- cum_RES_ig_boot  <- rep(NA_real_, B)

for (b in 1:B) {
  
  # resample residuals
  idx <- sample(1:(T-1), size = T-1, replace = TRUE)
  Ustar <- Uhat[idx, , drop = FALSE]
  
  # simulate Y*
  Ystar <- simulate_varx1(A1, Bx, Xmat, Ustar, y1 = Ymat[1, ])
  
  # re-estimate VARX
  vb <- try(VAR(Ystar, p = 1, type = "none", exogen = Xmat), silent = TRUE)
  if (inherits(vb, "try-error")) next
  
  # forecasts
  pb  <- try(predict(vb, n.ahead = H, dumvar = X_base),  silent = TRUE)
  pcp <- try(predict(vb, n.ahead = H, dumvar = X_cop),   silent = TRUE)
  pig <- try(predict(vb, n.ahead = H, dumvar = X_igrea), silent = TRUE)
  if (inherits(pb, "try-error") ||
      inherits(pcp, "try-error") ||
      inherits(pig, "try-error")) next
  
  # dynamic multipliers
  dm_REER_cp <- pcp$fcst$dREER[, "fcst"] - pb$fcst$dREER[, "fcst"]
  dm_RES_cp  <- pcp$fcst$dRES[,  "fcst"] - pb$fcst$dRES[,  "fcst"]
  dm_REER_ig <- pig$fcst$dREER[, "fcst"] - pb$fcst$dREER[, "fcst"]
  dm_RES_ig  <- pig$fcst$dRES[,  "fcst"] - pb$fcst$dRES[,  "fcst"]
  
  if (anyNA(c(dm_REER_cp, dm_RES_cp, dm_REER_ig, dm_RES_ig))) next
  
  # cumulative effects
  cum_REER_cop_boot[b] <- sum(dm_REER_cp)
  cum_RES_cop_boot[b]  <- sum(dm_RES_cp)
  cum_REER_ig_boot[b]  <- sum(dm_REER_ig)
  cum_RES_ig_boot[b]   <- sum(dm_RES_ig)
}




ci_table <- data.frame(
  series = c("Δln(COP) → Δln(REER)",
             "Δln(COP) → Δln(RES)",
             "ΔIGREA → Δln(REER)",
             "ΔIGREA → Δln(RES)"),
  lower_pct = 100 * c(
    quantile(cum_REER_cop_boot, 0.025, na.rm = TRUE),
    quantile(cum_RES_cop_boot,  0.025, na.rm = TRUE),
    quantile(cum_REER_ig_boot,  0.025, na.rm = TRUE),
    quantile(cum_RES_ig_boot,   0.025, na.rm = TRUE)
  ),
  upper_pct = 100 * c(
    quantile(cum_REER_cop_boot, 0.975, na.rm = TRUE),
    quantile(cum_RES_cop_boot,  0.975, na.rm = TRUE),
    quantile(cum_REER_ig_boot,  0.975, na.rm = TRUE),
    quantile(cum_RES_ig_boot,   0.975, na.rm = TRUE)
  )
)

print(ci_table)



####The dynamic multipliers suggest positive cumulative effects of copper price 
##shocks on both REER and international reserves, consistent with the spending effect and foreign exchange intervention hypothesis. However bootstrapped confidence intervals indicate these effects are not statistically significant at the 5% level, suggesting caution in causal interpretation. Results should be interpreted as indicative of the theorized
###Dutch Disease mechanisms rather than definitive confirmation#####
