#Carregando pacotes
install.packages('xlsx')
library(xlsx)
library(readr)
install.packages('readxl')
library(readxl)
library(dplyr)
install.packages("ggplot2")

#Estatística de Teste

#Hipóteses:
#𝐻0:𝜇0=15.4kg
#𝐻1:𝜇0≠15.4kg

xbar <- 14.1 # media da amostra
mu0 <- 15.4 # valor da hipótese
sigma <- 2 # desvio padrão da população
n <- 35 # tamanho da amostra
z <- (xbar - mu0)/(sigma/sqrt(n)) # estatística de teste
z
p = pnorm(-abs(z))
p

#Teste de Hipóteses

alpha = 0.05 #Nível de Significância
z.half.alpha = qnorm(1 - alpha/2) # Valor crítico
c(-z.half.alpha, z.half.alpha)


#Hipóteses:
#𝐻0:𝜇0=53kg
#𝐻1:𝜇0≠53kg


#Estatística de Teste:

xbar <- 50 # media da amostra 
mu0 <- 53 # valor da hipótese 
Var <- 16 #Variância
sigma <- sqrt(Var)
sigma <- # desvio padrão da população 
  n <- 15 # tamanho da amostra 
z <- (xbar - mu0)/(sigma/sqrt(n)) # estatística de teste
z# estatística de teste


#Teste de Hipóteses

alpha = 0.05 #Nível de Significância
z.half.alpha = qnorm(1 - alpha/2) # Valor crítico
c(-z.half.alpha, z.half.alpha)



#Hipóteses:
#𝐻0: 𝜇0 = 7.4𝑠
#𝐻1: 𝜇0 < 7.4𝑠


# teste unilateral
alpha = 0.01 #Nível de Significância
amostra <- c(6.8, 7.1, 5.9, 7.5, 6.3, 6.9, 7.2, 7.6, 6.6, 6.3)
n <- 10 # tamanho da amostra
df <- 9 # graus de liberdade
S <- sd(amostra); S  # desvio padrão da amostra
mu0 <- 7.4 # valor da hipótese
Var <- 1.3 #Variância
sigma = sqrt(Var)
xbar <- mean (amostra); xbar # média da amostra
z <- (xbar - mu0/(sigma/sqrt(n)))
z
p <- pnorm(-abs(z))
p
tc <- qt(alpha/2,df = df);tc # Equação para calculo de TC
Tt = (sqrt(n)*(xbar - mu0))/S; Tt# Equação da Estatística de Teste

alpha = 0.01 #Nível de Significância
z.half.alpha = qnorm(1 - alpha/2) # Valor crítico
c(-z.half.alpha, z.half.alpha)
