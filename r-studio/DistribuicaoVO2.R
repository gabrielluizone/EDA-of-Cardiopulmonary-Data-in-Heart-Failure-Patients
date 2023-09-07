# Instalações de Bibliotecas
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)
if(!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if(!require(psych)) install.packages('psych'); library(psych)
if(!require(knitr)) install.packages('knitr'); library(knitr)
if(!require(moments)) install.packages('moments'); library(moments)

# Carregamento de Dados e Tratamento
df <- read.csv('/cloud/project/data/R-Esforcor.csv', sep=';')

df$Paciente = as.character(df$Paciente)

df <- df %>%
  mutate_at(vars(all_of(c('Peso', 'Idade', 'VO2_REP', 'VO2_LAN', 'VO2_PCR', 'VO2_PIC'))),
                 ~ as.numeric(sub(',', '.', .)))

df$NYHA <- c(2,1,2,2,4,1,3,2,3,1,3,3,1,4,2,2,2,2,2,2,2,3,3,3,2,3,3,3,2,2,2,4,3,1,4,3,3,1,2,1,4,1,1,4,1,1,1,1,1,1,4,4,3,2,3,4,4,4,4,3,2,2,1,3,3,3,1,2,2,1,1,1,1,1,1,2,1,1,1,2,3,3,1,3,4,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

# Medidas de Tendência dos Dados
summary(df)
describe(df)

# Vizualização em Histograma
### Repouso
rep <- ggplot(data = df, aes(x = VO2_REP)) +
  geom_histogram(binwidth = 0.5, color = '#2a2f30', fill = '#454f52') +
  geom_vline(aes(xintercept = mean(VO2_REP)), color = '#9ed5e6', linetype = 'solid', size = 0.5) +
  labs(title = 'Histograma de Consumo de O2 em Repouso',
       x = 'Consumo (ml/kg.min)', y = 'Frequência') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))
ggsave("hist_rep.png", plot=rep, dpi = 600)

### Limiar Anaeróbio
lan <- ggplot(data = df, aes(x = VO2_LAN)) +
  geom_histogram(binwidth = 1, color = '#2a2f30', fill = '#454f52') +
  geom_vline(aes(xintercept = mean(VO2_LAN, na.rm=TRUE)), color = '#9ed5e6', linetype = 'solid', size = 0.5) +
  labs(title = 'Histograma de Consumo de O2 na Limiar Anaeróbio',
       x = 'Consumo (ml/kg.min)', y = 'Frequência') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))
ggsave("hist_lan.png", plot=lan, dpi = 600)

### Ponto de Compensação Respiratório
pcr <- ggplot(data = df, aes(x = VO2_PCR)) +
  geom_histogram(binwidth = 1.5, color = '#2a2f30', fill = '#454f52') + 
  geom_vline(aes(xintercept = mean(VO2_PCR, na.rm=TRUE)), color = '#9ed5e6', linetype = 'solid', size = 0.5) +
  labs(title = 'Hist. de Consumo de O2 no Ponto de Compensação Respiratório',
       x = 'Consumo (ml/kg.min)', y = 'Frequência') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))
ggsave("hist_pcr.png", plot=pcr, dpi = 600)

###Pico de Exercício
pic <- ggplot(data = df, aes(x = VO2_PIC)) +
  geom_histogram(binwidth = 2, color = '#2a2f30', fill = '#454f52') +
  geom_vline(aes(xintercept = mean(VO2_PIC)), color = '#9ed5e6', linetype = 'solid', size = 0.5) +
  labs(title = 'Histograma de Consumo de O2 no Pico de Exercício',
       x = 'Consumo (ml/kg.min)', y = 'Frequência') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))
ggsave("hist_pic.png", plot=pic, dpi = 600)

##################################################################

# Boxplot para VO2_REP
ggplot(data = df, aes(y = VO2_REP)) +
  geom_boxplot(color = '#2a2f30', fill = '#454f52') +
  labs(title = 'Boxplot de Consumo de O2 em Repouso',
       y = 'Consumo (ml/kg.min)') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))

# Boxplot para VO2_LAN
ggplot(data = df, aes(y = VO2_LAN)) +
  geom_boxplot(color = '#2a2f30', fill = '#454f52') +
  labs(title = 'Boxplot de Consumo de O2 na Limiar Anaeróbio',
       y = 'Consumo (ml/kg.min)') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))

# Boxplot para VO2_PCR
ggplot(data = df, aes(y = VO2_PCR)) +
  geom_boxplot(color = '#2a2f30', fill = '#454f52') +
  labs(title = 'Boxplot de Consumo de O2 no Ponto de Compensação Respiratório',
       y = 'Consumo (ml/kg.min)') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))

# Boxplot para VO2_PIC
ggplot(data = df, aes(y = VO2_PIC)) +
  geom_boxplot(color = '#2a2f30', fill = '#454f52') +
  labs(title = 'Boxplot de Consumo de O2 no Pico de Exercício',
       y = 'Consumo (ml/kg.min)') +
  theme(plot.title = element_text(size = 12, hjust = 0),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10))

######################################################################

df_box <- tidyr::pivot_longer(df, cols = starts_with("VO2"), names_to = "type")

# Criar o gráfico com facet_wrap
boxplot <- ggplot(data = df_box, aes(x = type, y = value)) +
  geom_boxplot(color = '#2a2f30', fill = '#454f52') +
  labs(title='Boxplots das Variáveis de Consumo de O2',
       x = 'Variável', y = 'Consumo (ml/kg.min)') +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 10)) +
  facet_wrap(~ type, scales = 'free', ncol = 2, 
  labeller=labeller(type=c(VO2_REP='Consumo de O2 em Repouso',
                           VO2_LAN='Consumo de O2 na Limiar Anaeróbio',
                           VO2_PCR='O2 no Ponto de Comp. Respiratório',
                           VO2_PIC='Consumo de O2 no Pico de Exercício')))
ggsave("Boxplot.png", plot=boxplot, dpi = 600)

######################################################################

table <- table(df$NYHA)

# Frequência relativa
freqrel <- round(table*100 / sum(table), 2)

# Frequência absoluta acumulada
frequencia_absoluta_acumulada <- cumsum(table)

# Frequência relativa acumulada
frequencia_relativa_acumulada <- round(cumsum(freqrel), 2)

nyha_table <- data.frame(NYHA = c(0, 1, 2, 3, 4),
              FreqAbsoluta = table,
              FreqRelativa = freqrel,
              FreqAbsAcum = frequencia_absoluta_acumulada,
              FreqRelAcum = frequencia_relativa_acumulada,
              Total= sum(table))

# Plotagem da Classe em Barras
barplot <- ggplot(nyha_table, aes(x = factor(NYHA), y = table, fill = factor(NYHA))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = table), vjust = -0.5, color = "black", size = 4) +
  labs(title = "Frequência das Classes Funcionais NYHA",
       x = "Classe Funcional NYHA",
       y = "Frequência") + ylim(0, 43) +
  scale_fill_manual(values = c("#59493b", "#3b5946", "#3b4659", "#593b3b", "#5c5c5c"),
                    name = "Classe Funcional NYHA",
                    guide = "none")
barplot
ggsave("Barplot.png", plot=barplot, dpi = 600)

# Plotagem em Gráfico de Setores
pizza <- ggplot(nyha_table, aes(x = '', y = FreqRelativa.Freq, fill=factor(NYHA))) +
  geom_bar(stat='identity', width=1) + coord_polar(theta='y') +
  labs(title = "Proporção das Classes Funcionais NYHA",
       fill = "Classe",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("#59493b", "#3b5946", "#3b4659", "#593b3b", "#5c5c5c")) +
  geom_text(aes(label = paste0(round(FreqRelativa.Freq, 1), " %")),
            position = position_stack(vjust = 0.5))
ggsave("Pizza.png", plot=pizza, dpi = 600)

######################################################################