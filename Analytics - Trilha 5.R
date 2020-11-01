library(readr) 
library(dplyr)
library(knitr) 
library(tidyverse)
library(ggplot2) 
library(ggpubr)
library(readxl) 
library(skimr) 
library(GGally)
library(data.table)
library(tibble) 
library(lubridate)
library(gplots)
library(psych)

#Prever o preço de carro a partir das seguintes variáveis: 
#potência (horsepower), 
#comprimento (length), 
#tamanho do motor (engine.size), 
#consumo na cidade (city.mpg).

#a)    Ajustar um modelo de regressão linear sendo price a variável alvo (resposta), 
#como função das demais variáveis citadas acima: horsepower, length, engine.size, city.mpg.

#importanto dados:
veiculo <- read.table("https://datahub.io/machine-learning/autos/r/autos.csv", header = TRUE, sep = ",", dec = ",")


#Visualizando o início e fim das observações:
head(veiculo)
tail(veiculo)

#Realizando sumário dos dados
skim(veiculo)

#Ajustando modelo de regressão linear
multivar <- data.frame(price = veiculo$price, 
                       horsepower = veiculo$horsepower, 
                       length = veiculo$length, 
                       engine.size = veiculo$engine.size, 
                       city.mpg = veiculo$city.mpg) 

ggcorr(multivar, palette = "RdylGn", name = bquote(rho), 
       label = TRUE, label_color = "black") +  
  theme(plot.caption = element_text(hjust = 0, size = 8))

#b)    Realizar a análise do modelo ajustado, avaliando o valor do R-quadrado, 
#a significância estatística de cada parâmetro ajustado 
#e a qualidade total do ajuste pela estatística F.

preco_veic <- lm(price ~ horsepower + length + engine.size + city.mpg, data = veiculo) 
summary(preco_veic)

ggpairs (multivar, columns = 1:ncol(multivar), title = "", axisLabels = "show") + 
  theme(axis.title.x = element_text(angle = 90, vjust = 0.5, size = 10)) + 
  theme_pubr() + labs_pubr() + 
  labs_pubr() + theme(plot.caption = element_text(hjust = 0, size = 8))

#c)    Realizar a verificação de aderência do modelo às premissas estatísticas do 
#método dos mínimos quadrados através dos gráficos diagnósticos, 
#comentando o gráfico dos resíduos x valores ajustados e o gráfico da curva Normal-QQ.

par(mfrow = c(2, 2)) 
plot(preco_veic)
