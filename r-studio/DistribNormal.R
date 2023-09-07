# Instalações de Bibliotecas
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)
if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(psych)) install.packages('psych'); library(psych)
if(!require(knitr)) install.packages('knitr'); library(knitr)
if(!require(moments)) install.packages('moments'); library(moments)

# Carregamento de Dados e Tratamento
df <- read.csv('/cloud/project/data/CompanyABCProfit.csv', sep=',')

sd <-sd(df$Profit.Rs..000.)
mean <- mean(df$Profit.Rs..000.)
n <- length(df$Profit.Rs..000.)

hist(df$Profit.Rs..000.)

min <- min(df$Profit.Rs..000.)
max <- max(df$Profit.Rs..000.)

densidade_normal <- function(x) dnorm(x, mean, sd)

# Plotar a densidade da distribuição normal
hist(df$Profit.Rs..000.)


curve(densidade_normal, from = min, to = max, col = "blue", lwd = 2,
      main = "Densidade da Distribuição Normal",
      xlab = "Valores", ylab = "Densidade")


pnorm(1000, mean, sd)
qnorm(0.95, mean = 100, sd = 10)

# p [250 < x < 750]
(pnorm( 750, mean, sd) - pnorm( 250, mean, sd))*100





