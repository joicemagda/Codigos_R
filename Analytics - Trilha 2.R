
#Carregando pacotes
install.packages('xlsx')
library(xlsx)
library(readr)
install.packages('readxl')
library(readxl)
library(dplyr)

#Carregando o arquivo PelicanStores.xlsx
pelican <- read_excel('PelicanStores.xlsx', "Página1")


# Visualizando o dataset
View(pelican)
head(pelican)
summary(pelican)

# Variável Itens
mean(pelican$Itens) #Média
median(pelican$Itens) #Mediana
sd (pelican$Itens) # Desvio PadrÃ£o
var (pelican$Itens) # Variância

#moda
tabela <- table(pelican$Itens)
View(tabela)
tabela[tabela == max(tabela)] #Retorna a moda da variável


# Variável Vendas Líquidas
mean(pelican$`Vendas líquidas`) #Média
median(pelican$`Vendas líquidas`) #Mediana
sd (pelican$`Vendas líquidas`) # Desvio PadrÃ£o
var (pelican$`Vendas líquidas`) # VariÃ¢ncia

#moda
tabela <- table(pelican$`Vendas líquidas`)
View(tabela)
tabela[tabela == max(tabela)]#Retorna a moda da Variável

# Variável Idade
mean(pelican$Idade) #Média
median(pelican$Idade) #Mediana
sd (pelican$Idade) # Desvio PadrÃ£o
var (pelican$Idade) # VariÃ¢ncia

#moda
tabela <- table(pelican$Idade)
View(tabela)
tabela[tabela == max(tabela)]#Retorna a moda da Variável

#Tabulação e Moda das Variáveis Categóricas
tabela <- table(pelican$`Tipo de Cliente`)
print(tabela)#Retorna o valor dos dados das variáveis
tabela[tabela == max(tabela)]#Retorna a moda do conjunto de dados

tabela <- table(pelican$`Método de Pagamento`)
print(tabela)#Retorna o valor dos dados das variáveis
tabela[tabela == max(tabela)]#Retorna a moda do conjunto de dados

tabela <- table(pelican$Gênero)
print(tabela)#Retorna o valor dos dados das variáveis
tabela[tabela == max(tabela)]#Retorna a moda do conjunto de dados

tabela <- table(pelican$`Estado Civil`)
print(tabela)#Retorna o valor dos dados das variáveis
tabela[tabela == max(tabela)]#Retorna a moda do conjunto de dados
