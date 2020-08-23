#Identificação da área de trabalho
getwd()

library(readr) 
library(dplyr)
library(knitr) 
library(tidyr) 
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

#lendo o arquivo csv
titanic_train <- read.csv("/cloud/project/train.csv", header = TRUE, stringsAsFactors = F, sep =",")
titanic_test <- read.csv("/cloud/project/test.csv", header = TRUE, stringsAsFactors = F, sep =",")

#conhecendo a estrutura dos dados
str(titanic_test)
str(titanic_train)

#Sumário dos dados
summary(titanic_test)
summary(titanic_train)


#As tarefas a serem realizadas são:

#A) Fazer a preparação dos dados para serem utilizados na análise, 
#considerando que serão utilizadas apenas as variáveis:
#survived, pclass, sex, age, sibsp, parch, fare, embarked
#i) Do conjunto de dados original, você deve selecionar um subconjunto apenas 
#com as variáveis indicadas acima e a variável passId. 

df_train <- data.frame(titanic_train[, c("PassengerId", "Survived", 
                                         "Pclass", "Sex", "Age", "SibSp", 
                                         "Parch", "Fare", "Embarked")], 
                       replace = TRUE, header = T, stringsAsFactors = F, sep = ",") 
ggcorr(df_train, palette = "RdYlGn", name = bquote(rho), label = TRUE, label_color = "black") + 
  theme(plot.caption = element_text(hjust = 0,size = 8))


#ii) Você deve atribuir um valor para os NAs na variável age. 
#Utilize algum critério razoável, por exemplo, o valor médio. 

table(is.na(df_train$Age)) 
m <- mean(df_train$Age, na.rm = TRUE) 
m 

df_train$Age[is.na(df_train$Age)] <- m 
describe(df_train$Age)

table(is.na(df_train$Age))


#iii) Você deve remover as linhas onde ainda estiverem faltando dados, 
#depois de atribuir o valor para os NAs de Age. 
#Poucas linhas estarão ainda com dados faltantes.


#B) Você deve criar um modelo onde survived será uma função das demais variáveis.

df_train$ynsurvived[df_train$Survived > 0] <- 1 
df_train$ynsurvived[df_train$Survived == 0] <- 0 
df_train$ynsurvived <- factor(df_train$ynsurvived, 
                              levels = c(0, 1), labels = c("No", "Yes")) 
table(df_train$ynsurvived)


fun_surv <- glm(ynsurvived ~ PassengerId + Pclass + Sex + Age + SibSp + 
                  Parch + Fare + Embarked, data = df_train, 
                family = binomial())


#C) Faça as análises do modelo, verificando a significância estatística 
#das variáveis (e seus parâmetros ajustados), gráficos diagnósticos, etc. 

#sumário da análise do modelo
summary(fun_surv)

#Gráfico do modelo
par(mfrow = c(2, 2)) 
plot(fun_surv)

#resumo dos passageiros
addmargins(table(df_train$ynsurvived))



p <- sum(df_train$ynsurvived == "Yes")/(length(df_train$ynsurvived)) 
p


odds_ynsurvived <- p/(1 - p) 
odds_ynsurvived


log10(odds_ynsurvived)



coef(fun_surv)



exp(coef(fun_surv))



#D) Atualize o modelo como consequência da análise realizada no item anterior.
surv_reduced <- glm(ynsurvived ~ Pclass + Sex + Age + SibSp, 
                    data = df_train, family = binomial()) 

summary(surv_reduced)



par(mfrow = c(2, 2)) 
plot(surv_reduced)



coef(surv_reduced)


exp(coef(surv_reduced))


anova(surv_reduced, fun_surv, test = "Chisq")


deviance(surv_reduced)/df.residual(surv_reduced)




#E) Faça as previsões da variável survived na base de dados de teste utilizando 
#o modelo refinado, e prepare um arquivo CSV para submissão que contenha apenas duas colunas: 
#passId, survived 
Survived <- df_train$ynsurvived 
PassId <- df_train$PassengerId 
Result <- data.frame(PassId, Survived) 

str(Result)


describe(Result)

