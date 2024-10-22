# Cvicenie 22.10.2024
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
#----