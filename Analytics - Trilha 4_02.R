install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(ggpubr)
library(readr)
library(dplyr)


#PROBLEMA 2: 
#O site Gapminder compilou uma base com dados sobre população, 
#expectativa de vida e PIB per capta de 142 países, em 12 anos diferentes. 
#Com estes dados, disponibilizados no arquivo pib_gapminder.csv, faça: 

#  a) Faça a importação dos dados, verifique a estrutura e faça um sumário estatístico.
dados <- read.table("http://www.leg.ufpr.br/~fernandomayer/data/pib_gapminder.csv", header = T, sep = ",", dec = ",")
View(dados)

#  b) Verifique a estrutura dos dados (str)
str(dados)

#  c) Classifique cada variável de acordo com seu tipo (qualitativa ordinal, nominal, quantitativa discreta, contínua, etc). 
#  d) Faça um sumário estatístico dos dados
summary(dados)

#  e) Faça uma tabela de frequência absoluta e uma tabela de frequência relativa 
#para verificar o número de observações por continente. 
df <- table(dados$continente)
df

df2 <- prop.table(df) * 100
df2

#  f) Faça um gráfico de barras da tabela de frequência absoluta dos continentes. 
bar <- data.frame(df) 
names(bar) <- c("Continentes", "Frequência") 
barplot(df, col = c("gray"), 
        ylim = c(0, 700), space = .1, width = c(.2, .2), 
        main = "Frequência por Continentes", 
        xlab = "Continentes", 
        ylab = "Frequência")


#  g) Faça um gráfico apropriado para relacionar o PIB per capta a expectativa de vida. 

plot(dados$pibPercap, 
     dados$expVida, 
     xlab = "PIB per Capta", 
     ylab = "Expectativa de Vida", 
     main = "Dispersão entre PIB e Expectativa de Vida", 
     col = c("gray", "blue", "red", "yellow", "magenta"))

plot(dados$pibPercap, 
     dados$expVida, 
     xlab = "PIB per Capta", 
     ylab = "Expectativa de Vida", 
     main = "Comparação PIB e Expectativa de Vida", 
     col = c("gray", "blue", "red", "yellow", "magenta"), 
     abline(lm(dados$expVida ~ dados$pibPercap), col="black"))

par(mfrow = c(2, 2)) 
PIB_ExpVida <- lm(pibPercap ~ expVida, data = dados) 
plot(PIB_ExpVida)


#  h) Crie duas novas colunas nesta base de dados com o logaritmo de PIB per capta, 
#e o logaritmo da expectativa de vida. Estas colunas devem ter os nomes: 
#lpibPercap e lexpVida, respectivamente.

lpibPercap <- log10(dados$pibPercap) 
dados['lpibPercap'] <- lpibPercap 
lexpVida <- log10(dados$expVida, base= 10)
dados['lexpVida'] <- lexpVida 
dados
