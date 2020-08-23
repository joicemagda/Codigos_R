
#Importando Bibliotecas
library("MASS")
library(class)
RNGversion('5.3.0') 

#Conhecendo os dados
help(biopsy)

#Sumário dos dados
summary(biopsy)

#Visualizando os dados em forma de tabela
View(biopsy)

#Removendo OS NA das linhas
biopsy_sem_NA_Linha <- na.omit(biopsy)

#Removendo OS NA das colunas
biopsy_sem_NA_coluna <- subset(biopsy, select=-V6)

#Sumário dos dados sem a coluna com NA
summary(biopsy_sem_NA_coluna)

#Definição da Acurácia
accuracy <- 0

#Laço FOR
for(i in 1:100) {
  
  #Selecionando a Amosta dos dados
  L <- sample(2:nrow(biopsy_sem_NA_coluna), 
              round(nrow(biopsy_sem_NA_coluna)/3))
  
  #Algoritmo de treino
  treino <- biopsy_sem_NA_coluna[-L, 2:9]
  
  #Algoritmo de teste
  teste <- biopsy_sem_NA_coluna[L, 2:9]
  
  #Classe dos dados de treino
  classe <- factor(biopsy_sem_NA_coluna[-L,10])
  
  #Definição do KNN
  fit_sem_NA_coluna <- knn(treino, teste, classe, k=5)
  
  #Definição da Matrix de confusão
  matrix_sem_NA_coluna <- table(fit_sem_NA_coluna[1:length(L)], 
                                factor(biopsy_sem_NA_coluna[L,10]))
  
  #Cálculo da Acurácia aplicado à cada variável da amostra
  cat('Accuracy: ', sum(diag(matrix_sem_NA_coluna))/ sum(matrix_sem_NA_coluna)*100, ' %' ,'\n' )
  
}
#Média da Acurácia
print(mean(accuracy))
