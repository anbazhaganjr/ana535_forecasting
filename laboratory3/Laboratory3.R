# ANA535 Lab 3 – Forecasting with Regression & Smoothing
# Originally written by Dr. Marvine Hamner
# Updated: 2025-05-02 by Naresh Anbazhagan
# Purpose: Full reproducibility, silent output capture, image export, log consolidation

# ======================
# Required Libraries
# ======================

reqd_pkgs <- c(
  "fpp3", "dplyr", "tidyverse", "ggplot2", "tsibble", "tsibbledata",
  "fable", "feasts", "lubridate", "zoo", "forecast", "TSA"
)

installed <- rownames(installed.packages())
to_install <- setdiff(reqd_pkgs, installed)
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(reqd_pkgs, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
cat("All libraries successfully loaded for Lab 3.\n\n")

# ======================
# Working Directory Setup
# ======================

setwd("/Users/anbzhnjr/learning/DataAnalytics/rprojects/ANA535/Lab3")
dir.create("data", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

# ======================
# Helper Functions
# ======================

save_plot <- function(filename, expr, width = 800, height = 600, res = 120) {
  png(file = paste0("output/plots/", filename), width = width, height = height, res = res)
  print(expr)
  dev.off()
  invisible(NULL)
}

start_log <- function(filename = "output/logs/ana535_lab3_output_log.txt") {
  while (sink.number() > 0) sink(NULL)
  sink(file = filename, append = FALSE, split = TRUE)
  start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Lab 3 Execution Started: ", start_time, " =====\n\n")
}

end_log <- function() {
  end_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  cat("\n===== ANA535 Lab 3 Execution Ended: ", end_time, " =====\n\n")
  sink()
}

log_section <- function(title) {
  cat("\n\n===== ", title, " =====\n\n")
}

reset_par <- function() {
  par(mfrow = c(1, 1))
}

# ======================
# Execution Begins
# ======================

start_log()
invisible(log_section("LAB 3 SCRIPT INITIALIZED"))
invisible(log_section("DATA LOADING AND INITIAL EXPLORATION"))

# Load the Amtrak dataset
amtrak_path <- "data/Amtrak1991-2024.csv"
if (!file.exists(amtrak_path)) {
  stop(paste("Data file not found at", amtrak_path))
}

Amtrak <- read.csv(amtrak_path)
colnames(Amtrak) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
Amtrak$Month <- lubridate::mdy(Amtrak$Month)

# Log structure and preview
str(Amtrak)
cat("\nFirst 5 rows of Amtrak data:\n")
print(head(Amtrak, 5))

# ======================
# Initial Time Plots
# ======================

invisible(log_section("INITIAL TIME PLOT WITH QUADRATIC TREND"))
reset_par()
save_plot("initial_quadratic_trend.png", {
  ggplot(Amtrak, aes(x = Month, y = PassengerMiles)) +
    geom_line(color = "blue") +
    stat_smooth(aes(y = PassengerMiles), method = "lm", formula = y ~ x + I(x^2)) +
    labs(title = "Amtrak Passenger Miles with Quadratic Trend")
})

invisible(log_section("CUBIC TREND FIT ON PASSENGER MILES"))
save_plot("initial_cubic_trend.png", {
  ggplot(Amtrak, aes(x = Month, y = PassengerMiles)) +
    geom_line(color = "blue") +
    stat_smooth(aes(y = PassengerMiles), method = "lm", formula = y ~ x + I(x^2) + I(x^3)) +
    labs(title = "Amtrak Passenger Miles with Cubic Trend")
})

# ======================
# Time Series Setup and Fits
# ======================

invisible(log_section("RIDERSHIP TIME SERIES INITIALIZATION"))
Amtrak.Ridership.ts <- ts(Amtrak$Ridership, start = c(1991, 1), end = c(2024, 6), frequency = 12)
invisible(str(Amtrak.Ridership.ts))

save_plot("ridership_timeseries_full.png", {
  plot(Amtrak.Ridership.ts,
       xlab = "Time", ylab = "Ridership", bty = "l",
       main = "Amtrak Ridership Time Series (1991–2024)")
})

invisible(log_section("RIDERSHIP REGRESSION FITS: QUADRATIC AND CUBIC"))
invisible(Amtrak.Ridership2.lm <- tslm(Amtrak.Ridership.ts ~ trend + I(trend^2)))
save_plot("ridership_quadratic_fit.png", {
  plot(Amtrak.Ridership.ts, xlab = "Time", ylab = "Ridership", bty = "l",
       main = "Amtrak Ridership with Quadratic Fit")
  lines(Amtrak.Ridership2.lm$fitted, lwd = 2, col = "#D55E00")
})

invisible(Amtrak.Ridership3.lm <- tslm(Amtrak.Ridership.ts ~ trend + I(trend^2) + I(trend^3)))
save_plot("ridership_cubic_fit.png", {
  plot(Amtrak.Ridership.ts, xlab = "Time", ylab = "Ridership", bty = "l",
       main = "Amtrak Ridership with Cubic Fit")
  lines(Amtrak.Ridership3.lm$fitted, lwd = 2, col = "#0072B2")
})


#
#***************************************** Step 1 ***********************#*

invisible(log_section("EDA: HISTOGRAM OF PASSENGERMILES"))

# Raw PassengerMiles histogram
save_plot("step1_hist_passengermiles_raw.png", {
  h <- hist(Amtrak$PassengerMiles, breaks = 40, density = 80,
            main = "Histogram of Amtrak Passenger Miles", col = "skyblue",
            xlab = "Passenger Miles")
  
  xfit <- seq(min(Amtrak$PassengerMiles), max(Amtrak$PassengerMiles), length = 100)
  yfit <- dnorm(xfit, mean = mean(Amtrak$PassengerMiles), sd = sd(Amtrak$PassengerMiles))
  yfit <- yfit * diff(h$mids[1:2]) * length(Amtrak$PassengerMiles)
  lines(xfit, yfit, col = "red", lwd = 2)
})

# Summary stats
cat("\nSummary of original PassengerMiles:\n")
summary(Amtrak$PassengerMiles)

invisible(log_section("EDA: HISTOGRAM OF SQUARED PASSENGERMILES"))

Amtrak$PassengerMiles_squared <- Amtrak$PassengerMiles^2

save_plot("step1_hist_passengermiles_squared.png", {
  h <- hist(Amtrak$PassengerMiles_squared, breaks = 40, density = 80,
            main = "Histogram of Squared Passenger Miles", col = "lightgreen",
            xlab = "Passenger Miles Squared")
  
  xfit <- seq(min(Amtrak$PassengerMiles_squared), max(Amtrak$PassengerMiles_squared), length = 100)
  yfit <- dnorm(xfit, mean = mean(Amtrak$PassengerMiles_squared), sd = sd(Amtrak$PassengerMiles_squared))
  yfit <- yfit * diff(h$mids[1:2]) * length(Amtrak$PassengerMiles_squared)
  lines(xfit, yfit, col = "red", lwd = 2)
})

cat("\nSummary of squared PassengerMiles:\n")
summary(Amtrak$PassengerMiles_squared)

invisible(log_section("EDA: TREND LINE FITS ON SQUARED PASSENGERMILES"))

# Linear trend on squared PassengerMiles
save_plot("step1_trendline_squared_linear.png", {
  ggplot(Amtrak, aes(x = Month, y = PassengerMiles_squared)) +
    geom_line(color = "blue") +
    stat_smooth(method = "lm", formula = y ~ x, color = "darkgreen") +
    labs(title = "Linear Trend: Squared Passenger Miles", x = "Month", y = "Passenger Miles²")
})

# Quadratic trend
save_plot("step1_trendline_squared_quadratic.png", {
  ggplot(Amtrak, aes(x = Month, y = PassengerMiles_squared)) +
    geom_line(color = "blue") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = "#D55E00") +
    labs(title = "Quadratic Trend: Squared Passenger Miles", x = "Month", y = "Passenger Miles²")
})

# Cubic trend
save_plot("step1_trendline_squared_cubic.png", {
  ggplot(Amtrak, aes(x = Month, y = PassengerMiles_squared)) +
    geom_line(color = "blue") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = "#0072B2") +
    labs(title = "Cubic Trend: Squared Passenger Miles", x = "Month", y = "Passenger Miles²")
})


#
#***************************************** Step 2 ***********************#*


invisible(log_section("STEP 2: MODEL FITTING AND RESIDUALS (QUADRATIC)"))

# Initialize PassengerMiles time series
Amtrak.ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2024, 6), frequency = 12)
invisible(str(Amtrak.ts))

# Fit quadratic regression model
Amtrak.lm <- tslm(Amtrak.ts ~ trend + I(trend^2))
cat("\nQuadratic Regression Model Summary:\n")
print(summary(Amtrak.lm))

# Plot full fit
save_plot("step2_passengermiles_fit_full.png", {
  plot(Amtrak.ts, xlab = "Time", ylab = "Passenger Miles", bty = "l", ylim = c(0, 1e9),
       main = "Passenger Miles and Quadratic Fit (Full Range)")
  lines(Amtrak.lm$fitted, col = "#D55E00", lwd = 2)
})

# Zoom in (1997–2000)
Amtrak.ts.zoom <- window(Amtrak.ts, start = c(1997, 1), end = c(2000, 12))

save_plot("step2_passengermiles_fit_zoom_1997_2000.png", {
  plot(Amtrak.ts.zoom, xlab = "Time", ylab = "Passenger Miles", bty = "l",
       main = "Passenger Miles Zoomed View (1997–2000)")
})


#
#***************************************** Step 3 ***********************#*

invisible(log_section("FITTING MULTIPLE REGRESSION MODELS WITH TSLM()"))

# Convert PassengerMiles to ts and then to tsibble
Amtrak.PassengerMiles.ts <- ts(Amtrak$PassengerMiles, start = c(1991, 1), frequency = 12)
Amtrak.tsb <- as_tsibble(Amtrak.PassengerMiles.ts)

# Fit linear, quadratic, cubic regression models with/without seasonality
fit_tslm <- Amtrak.tsb |>
  model(
    trend_fit       = TSLM(value ~ trend()),
    fit_tslm_season = TSLM(value ~ trend() + season()),
    fit_tslm2       = TSLM(value ~ trend() + season() + I(trend()^2)),
    fit_tslm3       = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3))
  )

# Log model summary
cat("\nModel Summary:\n")
print(report(fit_tslm))

# Plot residuals of all models
save_plot("step3_tslm_all_models_residuals.png", {
  augment(fit_tslm) |>
    autoplot(.resid) +
    labs(x = "Time", y = "", title = "Residuals from Multiple TSLM Fits")
})

# Log accuracy metrics sorted by MAPE
cat("\nAccuracy Summary (Sorted by MAPE):\n")
print(fit_tslm %>% accuracy() %>% arrange(MAPE))


invisible(log_section("FINAL CUBIC FIT VISUALIZATION (TSLM3)"))

# Refit only the cubic model (isolated)
fit_tslm <- Amtrak.tsb |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))

# Save plot of actual vs fitted values
save_plot("step3_tslm_cubic_fit_vs_actual.png", {
  augment(fit_tslm) |>
    ggplot(aes(x = index)) +
    geom_line(aes(y = value, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(
      y = NULL,
      title = "Amtrak Passenger Miles per Month (Cubic TSLM Fit)"
    ) +
    scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
    guides(colour = guide_legend(title = NULL))
})

invisible(log_section("PIECEWISE MODEL: 1991–1997"))

# Subset the time series and convert to tsibble
Amtrak.ts.91.97 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(1998, 1), frequency = 12)
Amtrak.tsb.91.97 <- as_tsibble(Amtrak.ts.91.97)

# Fit models for the period 1991–1997
fit_tslm_91_97 <- Amtrak.tsb.91.97 |>
  model(
    trend_fit = TSLM(value ~ trend()),
    fit_tslm_season = TSLM(value ~ trend() + season()),
    fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
    fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3))
  )

report(fit_tslm_91_97)

# Save plot: Fitted vs Actual (1991–1997)
save_plot("step3_tslm_fit_vs_actual_1991_1997.png", {
  augment(fit_tslm_91_97) |>
    ggplot(aes(x = index)) +
    geom_line(aes(y = value, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(
      y = NULL,
      title = "Amtrak Passenger Miles (1991–1997)"
    ) +
    scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
    guides(colour = guide_legend(title = NULL))
})


# ========== Residuals and Accuracy for 1991–1997 ==========
invisible(log_section("RESIDUALS AND ACCURACY: 1991–1997"))

# Plot residuals
save_plot("step3_residuals_1991_1997.png", {
  augment(fit_tslm_91_97) |>
    autoplot(.resid) +
    labs(x = "Time", y = "", title = "Residuals (1991–1997)")
})

# Accuracy measures
print(fit_tslm_91_97 %>% accuracy() %>% arrange(MAPE))

# Final cubic-only refit for cleaner plot
fit_tslm_91_97 <- Amtrak.tsb.91.97 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_97)

# Save actual vs fitted plot
save_plot("step3_tslm_cubic_fit_1991_1997.png", {
  augment(fit_tslm_91_97) |>
    ggplot(aes(x = index)) +
    geom_line(aes(y = value, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(y = NULL, title = "Cubic Fit: Passenger Miles (1991–1997)") +
    scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
    guides(colour = guide_legend(title = NULL))
})

# ========== Period 2: 1991–2004 ==========
invisible(log_section("PIECEWISE MODEL: 1991–2004"))

Amtrak.ts.91.04 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2005, 1), frequency = 12)
Amtrak.tsb.91.04 <- as_tsibble(Amtrak.ts.91.04)

fit_tslm_91_04 <- Amtrak.tsb.91.04 |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_04)

# Plot residuals
save_plot("step3_residuals_1991_2004.png", {
  augment(fit_tslm_91_04) |>
    autoplot(.resid) +
    labs(x = "Time", y = "", title = "Residuals (1991–2004)")
})

# Accuracy
print(fit_tslm_91_04 %>% accuracy() %>% arrange(MAPE))

# Cubic only
fit_tslm_91_04 <- Amtrak.tsb.91.04 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_04)

save_plot("step3_tslm_cubic_fit_1991_2004.png", {
  augment(fit_tslm_91_04) |>
    ggplot(aes(x = index)) +
    geom_line(aes(y = value, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(y = NULL, title = "Cubic Fit: Passenger Miles (1991–2004)") +
    scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
    guides(colour = guide_legend(title = NULL))
})

# ========== Period 3: 1991–2016 ==========
invisible(log_section("PIECEWISE MODEL: 1991–2016"))

Amtrak.ts.91.17 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.17 <- as_tsibble(Amtrak.ts.91.17)

fit_tslm_91_17 <- Amtrak.tsb.91.17 |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_17)

save_plot("step3_residuals_1991_2016.png", {
  augment(fit_tslm_91_17) |>
    autoplot(.resid) +
    labs(x = "Time", y = "", title = "Residuals (1991–2016)")
})

print(fit_tslm_91_17 %>% accuracy() %>% arrange(MAPE))

fit_tslm_91_17 <- Amtrak.tsb.91.17 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_17)

save_plot("step3_tslm_cubic_fit_1991_2016.png", {
  augment(fit_tslm_91_17) |>
    ggplot(aes(x = index)) +
    geom_line(aes(y = value, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(y = NULL, title = "Cubic Fit: Passenger Miles (1991–2016)") +
    scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
    guides(colour = guide_legend(title = NULL))
})

# ========== Period 4: 1991–2020 ==========
invisible(log_section("PIECEWISE MODEL: 1991–2020"))

Amtrak.ts.91.20 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2021, 1), frequency = 12)
Amtrak.tsb.91.20 <- as_tsibble(Amtrak.ts.91.20)

fit_tslm_91_20 <- Amtrak.tsb.91.20 |>
  model(trend_fit = TSLM(value ~ trend()),
        fit_tslm_season = TSLM(value ~ trend() + season()),
        fit_tslm2 = TSLM(value ~ trend() + season() + I(trend()^2)),
        fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_20)

save_plot("step3_residuals_1991_2020.png", {
  augment(fit_tslm_91_20) |>
    autoplot(.resid) +
    labs(x = "Time", y = "", title = "Residuals (1991–2020)")
})

print(fit_tslm_91_20 %>% accuracy() %>% arrange(MAPE))

fit_tslm_91_20 <- Amtrak.tsb.91.20 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_20)

save_plot("step3_tslm_cubic_fit_1991_2020.png", {
  augment(fit_tslm_91_20) |>
    ggplot(aes(x = index)) +
    geom_line(aes(y = value, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(y = NULL, title = "Cubic Fit: Passenger Miles (1991–2020)") +
    scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
    guides(colour = guide_legend(title = NULL))
})

#
#***************************************** Step 4 ***********************#*

invisible(log_section("STEP 4: TIME PLOT WITH CUBIC TREND (1991–2016)"))

Amtrak.ts.91.17 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.tsb.91.17 <- as_tsibble(Amtrak.ts.91.17)

save_plot("step4_cubic_trend_1991_2016.png", {
  ggplot(Amtrak, aes(x = Month, y = PassengerMiles)) +
    geom_line(color = "blue") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = "#D55E00") +
    labs(
      title = "Amtrak Passenger Miles (1991–2016) with Cubic Trend",
      x = "Time", y = "Passenger Miles"
    )
})


#
#***************************************** Step 5 ***********************#*

invisible(log_section("STEP 5: RESIDUAL DIAGNOSTICS AND DECOMPOSITION (1991–2016)"))

# Refit cubic model explicitly for 1991–2016 to match analysis
fit_tslm_91_17 <- Amtrak.tsb.91.17 |>
  model(fit_tslm3 = TSLM(value ~ trend() + season() + I(trend()^2) + I(trend()^3)))
report(fit_tslm_91_17)

# Residual diagnostics with gg_tsresiduals
save_plot("step5_gg_tsresiduals_1991_2016.png", {
  fit_tslm_91_17 |> gg_tsresiduals()
})

# ACF of full 1991–2016 tsibble
save_plot("step5_acf_passengermiles_1991_2016.png", {
  Amtrak.tsb.91.17 |>
    ACF() |> autoplot() +
    labs(subtitle = "Amtrak Passenger Miles (ACF: 1991–2016)")
})

# Decompose raw time series (not tsibble)
Amtrak.ts.91.16 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)
Amtrak.comp.91.16 <- decompose(Amtrak.ts.91.16)
save_plot("step5_decomposition_1991_2016.png", {
  autoplot(Amtrak.comp.91.16)
})

# First differencing: remove seasonal
Amtrak.ts.91.16_desea <- Amtrak.comp.91.16$x - Amtrak.comp.91.16$seasonal
Amtrak.ts.91.16_detren <- Amtrak.ts.91.16_desea - Amtrak.comp.91.16$trend
Amtrak.ts.91.16_de <- decompose(Amtrak.ts.91.16_detren)
save_plot("step5_decomposed_deseasoned_detrended.png", {
  autoplot(Amtrak.ts.91.16_de)
})

# ACF/PACF of differenced data
Amtrak.tsb.91.16_de <- as_tsibble(Amtrak.ts.91.16_detren)
save_plot("step5_acf_diff1_passengermiles.png", {
  Amtrak.tsb.91.16_de |>
    ACF() |> autoplot() +
    labs(subtitle = "Passenger Miles ACF (after 1st differencing)")
})

save_plot("step5_pacf_diff1_passengermiles.png", {
  Amtrak.tsb.91.16_de |>
    PACF() |> autoplot() +
    labs(subtitle = "Passenger Miles PACF (after 1st differencing)")
})

# Second differencing
Amtrak.ts.91.16_desea2 <- Amtrak.ts.91.16_de$x - Amtrak.ts.91.16_de$seasonal
save_plot("step5_second_differencing.png", {
  autoplot(Amtrak.ts.91.16_desea2)
})

# ACF of second differenced data
Amtrak.tsb.91.16_desea2 <- as_tsibble(Amtrak.ts.91.16_desea2)
save_plot("step5_acf_diff2_passengermiles.png", {
  Amtrak.tsb.91.16_desea2 |>
    ACF() |> autoplot() +
    labs(subtitle = "Passenger Miles ACF (after 2nd seasonal differencing)")
})

# Log-transform analysis
lnAmtrakPassengerMiles <- log(Amtrak$PassengerMiles)
lnAmtrak.ts <- ts(lnAmtrakPassengerMiles, start = c(1991, 1), end = c(2024, 6), frequency = 12)
save_plot("step5_log_passengermiles_timeseries.png", {
  plot(lnAmtrak.ts, xlab = "Time", ylab = "ln(Passenger Miles)", bty = "l",
       main = "Log of Passenger Miles")
})

# Decompose and plot log-transformed series
lnAmtrak.comp <- decompose(lnAmtrak.ts)
save_plot("step5_log_decomposition.png", {
  autoplot(lnAmtrak.comp)
})

# First seasonal differencing on log data
lnAmtrak_desea <- lnAmtrak.comp$x - lnAmtrak.comp$seasonal
lnAmtrak_desea.comp <- decompose(lnAmtrak_desea)
save_plot("step5_log_diff1_decomposition.png", {
  autoplot(lnAmtrak_desea.comp) +
    labs(subtitle = "Log Passenger Miles (1st seasonal differencing)")
})

# Second differencing on log data
lnAmtrak_desea2 <- lnAmtrak_desea.comp$x - lnAmtrak_desea.comp$seasonal
lnAmtrak_desea2.comp <- decompose(lnAmtrak_desea2)
save_plot("step5_log_diff2_decomposition.png", {
  autoplot(lnAmtrak_desea2.comp)
})

# ACF of log-transformed 2nd differenced
lnAmtrak_desea2.tsibble <- as_tsibble(Amtrak.ts.91.16_desea2)
save_plot("step5_log_diff2_acf.png", {
  lnAmtrak_desea2.tsibble |>
    ACF() |> autoplot() +
    labs(subtitle = "ACF: Log(Passenger Miles), 2nd differenced")
})

#**************************************** Step 6 *********************#*

Amtrak_3MA <- Amtrak.tsb.91.16_de

#
# Now work on the moving average smoothing beginning with the centered moving
# average
#

Amtrak_3MA.19.16 <- Amtrak_3MA |>
  mutate(
    `3-MA` = slider::slide_dbl(value, mean,
                               .before = 1, .after = 1, .complete = TRUE)
  )

save_plot(
  "step6_centered_moving_average.png",
  autoplot(Amtrak_3MA.19.16, value) +
    geom_line(aes(y = `3-MA`), colour = "#D55E00") +
    labs(y = "Passenger Miles",
         title = "Amtrak Passenger Miles by Month and Centered Moving Average")
)

log_section("STEP 6: ZOOM VIEW (2001–2003) - CENTERED 3-MA")

str(Amtrak.ts.91.16_detren)

Amtrak_MAZoom_2001_03 <- window(Amtrak.ts.91.16_detren,
                                start = c(2001, 1), end = c(2004, 1),
                                frequency = 12)
str(Amtrak_MAZoom_2001_03)

Amtrak.tsb_2001_03 <- as_tsibble(Amtrak_MAZoom_2001_03)

Amtrak_3MA_2001_03 <- Amtrak.tsb_2001_03 |>
  mutate(
    `3-MA` = slider::slide_dbl(value, mean,
                               .before = 1, .after = 1, .complete = TRUE)
  )

save_plot(
  "step6_zoom_centered_moving_average.png",
  autoplot(Amtrak_3MA_2001_03, value) +
    geom_line(aes(y = `3-MA`), colour = "#D55E00") +
    labs(y = "Passenger Miles",
         title = "Amtrak Passenger Miles by Month (2001–2003) with Centered 3-MA")
)

log_section("STEP 6: ZOOM VIEW (2001–2003) - TRAILING 3-MA")

Amtrak.ts_2001_03 <- ts(Amtrak$PassengerMiles, start = c(2001, 1), end = c(2004, 1), freq = 12)
Amtrak.tsb_2001_03 <- as_tsibble(Amtrak.ts_2001_03)

Amtrak_3MATR_2001_03 <- Amtrak.tsb_2001_03 |>
  mutate(
    `3-MA` = slider::slide_dbl(value, mean,
                               .before = 2, .after = 0, .complete = TRUE)
  )

save_plot(
  "step6_zoom_trailing_moving_average.png",
  autoplot(Amtrak_3MATR_2001_03, value) +
    geom_line(aes(y = `3-MA`), colour = "#D55E00") +
    labs(y = "Passenger Miles",
         title = "Amtrak Passenger Miles by Month (2001–2003) with Trailing 3-MA")
)

log_section("STEP 6: SHUMELI-STYLE 12-MA WITH 3-YEAR VALIDATION")

# Reload Amtrak clean for validation setup (preserves main ts object)
amtrak_path <- "data/Amtrak1991-2024.csv"
Amtrak_val <- read.csv(amtrak_path)
colnames(Amtrak_val) <- c("Month", "Ridership", "PassengerMiles", "RidersReported")
Amtrak_val$Month <- mdy(Amtrak_val$Month)

# Convert to per-thousand units for Ridership (as in original script)
Amtrak_val$Ridership <- Amtrak_val$Ridership / 1000

# Create time series: 1991–2004.25
ridership.ts <- ts(Amtrak_val$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

# Split train/test with 3-year validation
nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

# Compute 12-month trailing moving average
ma.trailing <- zoo::rollmean(train.ts, k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid),
                       start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid), freq = 12)

# Save base R plot as PNG
save_plot("step6_shumeli_12MA_validation.png", {
  plot(train.ts, ylim = c(1300, 2600), ylab = "Ridership (Thousands)",
       xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991, 2006.25), main = "")
  axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
  lines(ma.trailing, lwd = 2)
  lines(ma.trailing.pred, lwd = 2, col = "blue", lty = 2)
  lines(valid.ts)
  abline(v = c(2001.25, 2004.25), lty = 2)
  text(1996.25, 2500, "Training")
  text(2002.75, 2500, "Validation")
  text(2005.25, 2500, "Future")
  arrows(2001.25, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)
  arrows(2001.25, 2450, 2004.25, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)
  arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)
})

log_section("STEP 6: SEASONAL NAIVE FORECAST")

# Plot seasonal naive model from original ts object (PassengerMiles)
AmtrakSNaive <- window(Amtrak.ts, start = c(1991, 1), end = c(2024, 6))

save_plot("step6_seasonal_naive_forecast.png", {
  autoplot(AmtrakSNaive) +
    autolayer(snaive(AmtrakSNaive), h = 36,
              series = "Seasonal naive", PI = FALSE) +
    ggtitle("Amtrak Passenger Miles by Month and Seasonal Naive Forecast") +
    xlab("Time") + ylab("Passenger Miles") +
    guides(colour = guide_legend(title = "Forecast"))
})

#**************************************** Step 7 *********************#*

log_section("STEP 7: EXPONENTIAL SMOOTHING (ETS)")

# Subset the PassengerMiles time series from 1991 to 2019
Amtrak1991_2019 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2019, 1), frequency = 12)

# Fit ETS model
fit_ets <- ets(Amtrak1991_2019)
cat("\nETS Model Summary (1991–2019):\n")
print(fit_ets)

# Save plot of fitted model
save_plot("step7_ets_fit_1991_2019.png", {
  autoplot(fit_ets) +
    labs(title = "ETS Model Fit: Amtrak Passenger Miles (1991–2019)")
})

# Forecast 60 months ahead and plot
fit_ets_fcast <- forecast(fit_ets, h = 60)
save_plot("step7_ets_forecast_1991_2019.png", {
  autoplot(fit_ets_fcast) +
    ylab("Amtrak Passenger Miles by Month") +
    labs(title = "ETS Forecast: 1991–2019 with 5-Year Projection")
})

# Now create a second model for 2020–2024.6
Amtrak2024 <- ts(Amtrak$PassengerMiles, start = c(2020, 1), end = c(2024, 6), frequency = 12)

fit_ets2 <- ets(Amtrak2024)
cat("\nETS Model Summary (2020–2024):\n")
print(fit_ets2)

# Save fit and forecast plot
save_plot("step7_ets_fit_2020_2024.png", {
  autoplot(fit_ets2) +
    labs(title = "ETS Model Fit: Amtrak Passenger Miles (2020–2024)")
})

fit_ets2_fcast <- forecast(fit_ets2, h = 60)
save_plot("step7_ets_forecast_2020_2024.png", {
  autoplot(fit_ets2_fcast) +
    ylab("Amtrak Passenger Miles by Month") +
    labs(title = "ETS Forecast: 2020–2024 with 5-Year Projection")
})

# Overlay fitted vs forecasted series for 1991–2019
save_plot("step7_ets_overlay_1991_2019.png", {
  autoplot(fit_ets$fitted) +
    autolayer(fit_ets_fcast$mean, series = "Forecast") +
    ylab("Passenger Miles") +
    labs(title = "ETS Fitted vs Forecast (1991–2019)")
})

# Overlay fitted vs forecasted series for 2020–2024
save_plot("step7_ets_overlay_2020_2024.png", {
  autoplot(fit_ets2$fitted) +
    autolayer(fit_ets2_fcast$mean, series = "Forecast") +
    ylab("Passenger Miles") +
    labs(title = "ETS Fitted vs Forecast (2020–2024)")
})

log_section("STEP 8: HOLT-WINTERS FORECASTING (1991–2019)")

# Re-initialize series from 1991 to 2019
Amtrak1991_2019 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2019, 1), frequency = 12)

# Fit Holt-Winters models with additive and multiplicative seasonality
fit1_hw <- hw(Amtrak1991_2019, seasonal = "additive", h = 96)
fit2_hw <- hw(Amtrak1991_2019, seasonal = "multiplicative", h = 96)

cat("\nHolt-Winters Additive Model:\n")
print(fit1_hw)

cat("\nHolt-Winters Multiplicative Model:\n")
print(fit2_hw)

# Save overlay plot comparing both forecasts
save_plot("step8_hw_additive_vs_multiplicative.png", {
  autoplot(Amtrak1991_2019) +
    autolayer(fit1_hw, series = "HW additive forecasts", PI = FALSE) +
    autolayer(fit2_hw, series = "HW multiplicative forecasts", PI = FALSE) +
    xlab("Time") +
    ylab("Passenger Miles") +
    ggtitle("Amtrak Passenger Miles by Month") +
    guides(colour = guide_legend(title = "Forecast"))
})

# Accuracy of both models
cat("\nAccuracy: Holt-Winters Additive\n")
print(accuracy(fit1_hw))

cat("\nAccuracy: Holt-Winters Multiplicative\n")
print(accuracy(fit2_hw))

log_section("STEP 8: HOLT-WINTERS FORECASTING (2020–2024)")

# Initialize time series from Jan 2020 to Jun 2024
Amtrak2024 <- ts(Amtrak$PassengerMiles, start = c(2020, 1), end = c(2024, 6), frequency = 12)

# Fit Holt-Winters models (Additive and Multiplicative)
fit3_hw <- hw(Amtrak2024, seasonal = "additive", h = 36)
fit4_hw <- hw(Amtrak2024, seasonal = "multiplicative", h = 36)

cat("\nHolt-Winters Additive Model (2020–2024):\n")
print(fit3_hw)

cat("\nHolt-Winters Multiplicative Model (2020–2024):\n")
print(fit4_hw)

# Save comparative plot of the two models
save_plot("step8_hw_2020_2024_forecasts.png", {
  autoplot(Amtrak2024) +
    autolayer(fit3_hw, series = "HW additive forecasts", PI = FALSE) +
    autolayer(fit4_hw, series = "HW multiplicative forecasts", PI = FALSE) +
    xlab("Time") +
    ylab("Passenger Miles") +
    ggtitle("Amtrak Passenger Miles by Month") +
    guides(colour = guide_legend(title = "Forecast"))
})

# Accuracy reporting
cat("\nAccuracy: Holt-Winters Additive (2020–2024)\n")
print(accuracy(fit3_hw))

cat("\nAccuracy: Holt-Winters Multiplicative (2020–2024)\n")
print(accuracy(fit4_hw))

log_section("STEP 8: HOLT-WINTERS COMPARISON AND HISTORICAL FITS")

# Compare all four HW models on a unified plot (zoom into 2024–2027)
save_plot("step8_hw_comparison_2024_onward.png", {
  autoplot(Amtrak1991_2019) +
    autolayer(fit1_hw, series = "HW Fit 1", PI = FALSE) +
    autolayer(fit2_hw, series = "HW Fit 2", PI = FALSE) +
    autolayer(fit3_hw, series = "HW Fit 3", PI = FALSE) +
    autolayer(fit4_hw, series = "HW Fit 4", PI = FALSE) +
    xlab("Time") +
    ylab("Passenger Miles") +
    ggtitle("Amtrak Passenger Miles by Month (Holt-Winters Comparisons)") +
    coord_cartesian(xlim = c(2024, 2027)) +
    guides(colour = guide_legend(title = "Forecast"))
})

cat("\nComparison of HW Fit 1–4 plotted from 2024 onward.\n")

# Historical forecast prior to pandemic using 1991–2016 data
Amtrak1991_2016 <- ts(Amtrak$PassengerMiles, start = c(1991, 1), end = c(2017, 1), frequency = 12)

fit_hw   <- hw(Amtrak1991_2016)                        # default
fit_hwA  <- hw(Amtrak1991_2016, seasonal = "additive")
fit_hwM  <- hw(Amtrak1991_2016, seasonal = "multiplicative")

# Print summaries
cat("\nSummary: HW (default)\n")
print(summary(fit_hw))

cat("\nSummary: HW Additive\n")
print(summary(fit_hwA))

cat("\nSummary: HW Multiplicative\n")
print(summary(fit_hwM))

log_section("STEP 8: ADDITIVE VS MULTIPLICATIVE HW PLOTS")

# Plot additive and multiplicative HW side-by-side
save_plot("step8_hw_add_vs_mult.png", {
  autoplot(Amtrak1991_2016) +
    autolayer(fit_hwA, series = "HW additive forecasts", PI = FALSE) +
    autolayer(fit_hwM, series = "HW multiplicative forecasts", PI = FALSE) +
    xlab("Time") +
    ylab("Passenger Miles") +
    ggtitle("Amtrak Passenger Miles by Month (HW Additive vs Multiplicative)") +
    guides(colour = guide_legend(title = "Forecast"))
})

# Additive-only
save_plot("step8_hw_add_only.png", {
  autoplot(Amtrak1991_2016) +
    autolayer(fit_hwA, series = "HW additive forecasts", PI = FALSE) +
    xlab("Time") + ylab("Passenger Miles") +
    ggtitle("Amtrak Passenger Miles (HW Additive Only)") +
    guides(colour = guide_legend(title = "Forecast"))
})

# Multiplicative-only
save_plot("step8_hw_mult_only.png", {
  autoplot(Amtrak1991_2016) +
    autolayer(fit_hwM, series = "HW multiplicative forecasts", PI = FALSE) +
    xlab("Time") + ylab("Passenger Miles") +
    ggtitle("Amtrak Passenger Miles (HW Multiplicative Only)") +
    guides(colour = guide_legend(title = "Forecast"))
})

# Accuracy metrics
cat("\nAccuracy - HW Additive:\n")
print(accuracy(fit_hwA))

cat("\nAccuracy - HW Multiplicative:\n")
print(accuracy(fit_hwM))

#
#***************************************** Step 8 ***********************#*

log_section("STEP 8: SIMPLE EXPONENTIAL SMOOTHING ON DIFFERENCED RIDERSHIP")

# Two-stage differencing of ridership
diff.twice.ts <- diff(diff(ridership.ts, lag = 12), lag = 1)

# Validation splits
nValid <- 36
nTrain <- length(diff.twice.ts) - nValid
train.ts <- window(diff.twice.ts, start = c(1992, 2), end = c(1992, nTrain + 1))
valid.ts <- window(diff.twice.ts, start = c(1992, nTrain + 2), end = c(1992, nTrain + 1 + nValid))

# SES with alpha = 0.2
ses <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.pred <- forecast(ses, h = nValid, level = 0)

save_plot("step8_ses_fixed_alpha.png", {
  plot(ses.pred, ylim = c(-250, 300), ylab = "Ridership (Twice-Differenced)",
       xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991, 2006.25), main = "", flty = 2)
  axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
  lines(ses.pred$fitted, lwd = 2, col = "blue")
  lines(valid.ts)
  lines(c(2004.25 - 3, 2004.25 - 3), c(-250, 350))
  lines(c(2004.25, 2004.25), c(-250, 350))
  text(1996.25, 275, "Training")
  text(2002.75, 275, "Validation")
  text(2005.25, 275, "Future")
  arrows(2004 - 3, 245, 1991.5, 245, code = 3, length = 0.1, lwd = 1, angle = 30)
  arrows(2004.5 - 3, 245, 2004, 245, code = 3, length = 0.1, lwd = 1, angle = 30)
  arrows(2004.5, 245, 2006, 245, code = 3, length = 0.1, lwd = 1, angle = 30)
})

# Optimized SES
ses.opt <- ets(train.ts, model = "ANN")
ses.opt.pred <- forecast(ses.opt, h = nValid, level = 0)

cat("\nSES Optimized Summary:\n")
print(ses.opt)

cat("\nAccuracy - Fixed Alpha SES:\n")
print(accuracy(ses.pred, valid.ts))

cat("\nAccuracy - Optimized SES:\n")
print(accuracy(ses.opt.pred, valid.ts))

end_log()

