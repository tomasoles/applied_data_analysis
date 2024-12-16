# Prenaska/ cvicenie 22.10.2024 a 10.12.2024
tlak <- read.csv("tlak.csv")
library(ggplot2) 
library(dplyr)
#--- Histogram tlak ----
tlak %>% as.matrix() %>% as.vector() %>% na.omit() %>%
  #hist()
  hist(main = "", 
       xlab = "tlak", ylab = "početnosť", 
       col = "blue", 
       breaks = 20)

  #ggplot() +
  #geom_histogram(aes(x = ?), binwidth = 0.5, fill = "blue", color = "black")

#---- Histogram + Empiricka CDF ----
# Data preparation
tlak_data <- as.vector(as.matrix(tlak))
tlak_data <- na.omit(tlak_data)
df <- data.frame(tlak_data)

# Fig 1: Histogram
hist_plot <- ggplot(df, aes(x = tlak_data)) +
  geom_histogram(fill = "blue", bins = 20) +
  labs(title = "Histogram of Tlak Data", x = "tlak", y = "Frequency") +
  theme_minimal()

# Fig 2: Empirical CDF
cdf_plot <- ggplot(df, aes(x = tlak_data)) +
  stat_ecdf(geom = "step", color = "blue", size = 0.9) +
  labs(title = "Empirical CDF of Tlak Data", x = "tlak", y = "Cumulative Probability") +
  theme_minimal()

# Kombinovany graf 2 vedla seba s medzerou
#install.packages("patchwork")
library(patchwork)
combined_plot <- hist_plot + plot_spacer() + cdf_plot + plot_layout(ncol = 3, widths = c(1, 0.15, 1))
combined_plot
#---- Grafy 6x PDF ----
curve(dchisq(x, df = 3), from = 0, to = 5, ylim = c(0, 0.6),
      col = "blue", lwd = 3, ylab = "f(x)", 
      xlab = "x", main = "chi-kvadrát (dchisq)")
curve(dexp(x, rate = 2), from = 0, to = 5, ylim = c(0, 0.6),
      col = "blue", lwd = 3, ylab = "f(x)", 
      xlab = "x", main = "exponenciálne (dexp)")

curve(dgamma(x, 2, 1, 1), from = 0, to = 5, ylim = c(0, 0.6),
      col = "blue", lwd = 3, ylab = "f(x)", 
      xlab = "x", main = "gamma (dgamma)")

curve(dcauchy(x, 3, 0.6), from = 0, to = 5, ylim = c(0, 0.6),
      col = "blue", lwd = 3, ylab = "f(x)", 
      xlab = "x", main = "Cauchyho (dcauch)")

curve(dt(x, df = 4), from = -3, to = 3, ylim = c(0, 0.6),
      col = "blue", lwd = 3, ylab = "f(x)", 
      xlab = "x", main = "Studentovo (dt)")

curve(dnorm(x, 1, 0.7), from = -2, to = 3, ylim = c(0, 0.6),
      col = "blue", lwd = 3, ylab = "f(x)", 
      xlab = "x", main = "Gaussovo (dnorm)")
#---- PDF, CDF, kvantily, Var, stredna hodnota ----
dnorm(0)
pnorm(2)
qnorm(0.025)
u <- rnorm(500)
hist(u)
hist(u^2)
var1 = mean((u-mean(u))^2); var1
var2 = mean(u^2)-mean(u)^2; var2
#---- Korelacie v regresii ----
m_ols2 <- lm(wage ~ educ + IQ + exper + married, wage)
summary(m_ols2)
y_hat <- predict(m_ols2); u_hat <- resid(m_ols2)
#cor, cov, var, sd
educ_p_value <- summary(m_ols2)$coefficients["educ", 4]
educ_point <- summary(m_ols2)$coefficients["educ", 1]
#dt(), pt(), qt()

#---- Numericky integral, pravidlo sigma ----
integrate(function(x) {x^2}, lower = 0, upper = 1)

f_norm <- function(x, mu = 2, sigma = 3) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-((x - mu)^2 / (2 * sigma^2))) }

f_norm_std <- function(z) {(1 / sqrt(2 * pi)) * exp(-0.5 * z^2)}

integrate(f_norm_std, lower = -Inf, upper = -2)
integrate(f_norm, lower = -Inf, upper = -4)
