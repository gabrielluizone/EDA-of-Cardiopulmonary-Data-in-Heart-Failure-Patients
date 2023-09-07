# Instalações de Bibliotecas
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)
if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(readxl)) install.packages('readxl'); library(readxl)

cor <- read_excel('/cloud/project/data/coronarias.xlsx')
ml <- read.csv('/cloud/project/data/milsa.csv')

# Alterando os "." para NaN
cor$TABAG4 <- replace(cor$TABAG4, cor$TABAG4 == '.', NA)
cor$ARTER <- replace(cor$ARTER, cor$ARTER == '.', NA)

########################################################################
# Tabbela de contingência
########################################################################
## TABAG4 - TABAGISMO    | 4 CATEGORIAS
## ARTER  - ARTERIOPATIA | 0-NENHUMA | 1-CAROT | 2-AORT | 3-CAROT+AORT
########################################################################
# O atributo tabagimo, conforme o dicionário, foi determinada que possui
# somente 4 categorias, entretanto, o conjunto de dados possui além de
# 4 categorias, especificamente 6 categorias, nas quais possuem uma
# frequência muito baixa em relação a outras categorias
########################################################################
tab <- table(cor$TABAG4, cor$ARTER) # Linha, Coluna
rownames(tab) <- c('Nível 1', 'Nível 2', 'Nível 3', 'Nível 4', 'Nível ?', 'Nível ??') # Tabag4
colnames(tab) <- c('Nenhuma', 'Carot', 'Aort', 'Corato+Aort') # Arter
tab

########################################################################
# 
########################################################################