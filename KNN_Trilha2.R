
#Importando Bibliotecas
library("MASS")
library(class)
RNGversion('5.3.0') 

#Conhecendo os dados
help(biopsy)

#Sumário dos dados bruto
summary(biopsy)

#Visualizando os dados em forma de tabela
View(biopsy)

#Removendo OS NA das linhas
biopsy_sem_NA_Linha <- na.omit(biopsy)

#Removendo OS NA das colunas
biopsy_sem_NA_coluna <- subset(biopsy, select=-V6)

#Sumário dos dados sem NA nas linhas
summary(biopsy_sem_NA_Linha)

#Definição da Acurácia
accuracy <- 0

#Laço FOR
for(i in 1:100) {
  
  #Selecionando a Amosta dos dados
  amostra <- sample(1:nrow(biopsy_sem_NA_Linha), 
                    round(nrow(biopsy_sem_NA_Linha)/3))
  
  #Algoritmo de treino
  treino <- biopsy_sem_NA_Linha[-amostra, 2:10]
  
  #Algoritmo de teste
  teste <- biopsy_sem_NA_Linha[amostra, 2:10]
  
  #Classe dos dados de treino
  classe <- factor(biopsy_sem_NA_Linha[-amostra,11])
  
  #Definição do KNN
  fit_sem_NA_Linha <- knn(treino, teste, classe, k=12)
  
  #Definição da Matrix de confusão
  matrix_sem_NA_linha <- table(fit_sem_NA_Linha[1:length(amostra)], 
                               factor(biopsy_sem_NA_Linha[amostra,11]))
  
  #Cálculo da Acurácia aplicado à cada variável da amostra
  cat('Accuracy: ', sum(diag(matrix_sem_NA_linha))/ sum(matrix_sem_NA_linha)*100, ' %' ,'\n' )
  
}
#Média da Acurácia
print(mean(accuracy))

