install.packages("ggplot2")
install.packages ("dplyr")
install.packages("tidyverse")
install.packages("skimr")
install.packages("gridExtra")

library(ggplot2)
library(ggpubr)
library(data.table)
library(dplyr)
library(tidyr)
library(gapminder)
library(skimr)
library(gridExtra)


#Carregando dados
data("diamonds")

#Transferindo dados para uma variável
diamantes <- diamonds

#Visualizando dados da Variável
diamantes

#Verificando estrutura dos dados
str(diamantes)

#Parte Inicial do conjunto de dados
head(diamantes)

#Parte Final do conjunto de dados
tail(diamantes)

#Sumários estatísticos para entender a base de dados. 
fivenum(diamantes$carat)
fivenum(diamantes$depth)
fivenum(diamantes$price)
fivenum(diamantes$x)
fivenum(diamantes$y)

skim(diamantes)

#A saída da função summary() estão de acordo 
#com a descrição mostrada anteriormente?
summary(diamantes)


#Explore a variável price, seguindo o modelo de exploração.
#Organizando os dados de acordo com a variável Price (Cresc)
diamantes <- arrange(diamantes, price)
diamantes

#Selecionando as variáveis para observação
AvalPrice <- select(diamantes,carat, cut,price)
AvalPrice

#Exibir Grupo de Boxplot
AvalPrice %>% 
  filter(cut %in% c("Ideal","Premium","Good", "Very Good", "Fair")) %>%
  ggplot(aes(x=price, y=carat, fill=factor(cut))) +
  geom_boxplot() 

#Sumário da observação das três variáveis.
summary(AvalPrice)

#Distribuição da variável (histograma); 
#Observe a faixa de valores da variável
hist(AvalPrice$price, col = "green",breaks = 20, main = "Variável Price", xlab = "price",
     ylab = "frequência")

#Exploração das variáveis carat, cut, color, clarity, x, y, z, depth e table
AvalPrice <- select(diamantes,carat, cut, clarity,price)
head(AvalPrice)
as.data.frame(AvalPrice)  


carat <- ggplot(data = diamantes) + geom_histogram(aes(carat)) + 
  labs_pubr() + theme_pubclean() 

depth <- ggplot(data = diamantes) + geom_histogram(aes(depth)) + 
  labs_pubr() + theme_pubclean() 

table <- ggplot(data = diamantes) + 
  geom_histogram(aes(table)) + labs_pubr() + theme_pubclean() 

price <- ggplot(data = diamantes) + geom_histogram(aes(price)) + 
  labs_pubr() + theme_pubclean() 

x <- ggplot(data = diamantes) + 
  geom_histogram(aes(x)) + labs_pubr() + theme_pubclean() 

y <- ggplot(data = diamantes) + geom_histogram(aes(y)) + labs_pubr() + 
  theme_pubclean() 

z <- ggplot(data = diamantes) +
  geom_histogram(aes(z)) + labs_pubr() + theme_pubclean() 

ggarrange(ncol = 4, nrow = 3, carat, depth, table, price, x, y, z)

#Boxplots para variaveis numéricas
carat <- ggplot(data = diamantes) + geom_boxplot(aes(y = carat)) + labs_pubr() +
  theme_pubclean()

depth <- ggplot(data = diamantes) + geom_boxplot(aes(y = depth)) + labs_pubr() +
  theme_pubclean()

table <- ggplot(data = diamantes) + geom_boxplot(aes(y = table)) + labs_pubr() +
  theme_pubclean()

price <- ggplot(data = diamantes) + geom_boxplot(aes(y = price)) + labs_pubr() +
  theme_pubclean()

x <- ggplot(data = diamantes) + geom_boxplot(aes(y = x)) + labs_pubr() +
  theme_pubclean()

y <- ggplot(data = diamantes) + geom_boxplot(aes(y = y)) + labs_pubr() +
  theme_pubclean()

z <- ggplot(data = diamantes) + geom_boxplot(aes(y = z)) + labs_pubr() +
  theme_pubclean()

ggarrange(ncol = 4, nrow = 3, carat, depth, table,price,x, y, z)


#Facetamento dos dados das variáveis categóricas. 
#Gráficos com 2 ou mais variáveis continuas lado a lado.

cut_bar <- ggplot(diamantes) + 
  geom_bar(aes(x = cut), stat = "count") + 
  labs_pubr() + theme_pubclean() 
color_bar <- ggplot(diamantes) + 
  geom_bar(aes(x = color), stat = "count") + 
  labs_pubr() + theme_pubclean() 
clarity_bar <- ggplot(diamantes) + 
  geom_bar(aes(x = clarity), stat = "count") + 
  labs_pubr() + theme_pubclean() 

ggarrange(ncol = 1, nrow = 3, cut_bar, color_bar, clarity_bar)
