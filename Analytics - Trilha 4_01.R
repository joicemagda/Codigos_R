install.packages("ggplot2")
library(ggplot2)
library(ggpubr)

#Problema 1

#Para uma amostra de oito operadores de m?quina, 
#foram coletados o número de horas de treinamento (x) 
#e o tempo necess?rio para completar o trabalho (y). 
#Os dados coletados encontram-se na tabela abaixo:

#Dados de X
hora <- c(5.2,5.1,4.9,4.6,4.7,4.8,4.6,4.9)

#Dados de Y
tempo <- c(13,15,16,20,19,17,21,16)

dados <- data.frame(tempo, hora)
dados

#Estrutura dos dados
str(dados)


names(dados)
X <- dados$hora
Y <- dados$tempo
n <- length(X)


# a) Um gráfico de dispersão para os dados
p <- ggplot (data = dados, aes(x=hora, y=tempo)) +
  geom_point(color = "red", fill="blue") + xlab("Hora") +
  ylab("Tempo") + theme_pubr(legend = "right") +
  labs_pubr() + theme(plot.caption = element_text(hjust = 0))


#Exibição do gráfico de dispersão
p


# b) Determine o modelo de regressão linear simples entre as variáveis x e y
# sendo y a variável resposta.

regressao <- lm (hora ~ tempo, data = dados)
regressao

# c) Faça uma análise do modelo de regressão utilizando a função summary:
# i) resíduos, significância estatística dos coeficientes, 
#    percentual de variância explicada pelo modelo.

summary(regressao)


# d) Trace, no gráfico anterior, a reta de regressão.

(SX <- sum(X))
(SSX <- sum((X - mean(X))^2))
(SY <- sum(Y))
(SSY <- sum((Y - mean(Y))^2))
(SSXY <- sum((X - mean(X))*(Y - mean(Y))))
(b_1 <- SSXY/SSX)
(b_0 <- mean(Y) - b_1*mean(X))

p <- ggplot(data = dados, aes(x=hora, y=tempo)) +
  geom_point(color = "red", fill="blue") + xlab("Hora") +
  ylab("Tempo") + theme_pubr(legend = "right") + labs_pubr() +
  labs_pubr() + theme(plot.caption = element_text(hjust = 0))
p <- p + geom_abline(intercept = b_0, slope = b_1, color = "blue")
p

