#instalar biblioteca nova:
install.packages("RWeka")
install.packages("readxl")
install.packages("reshape2")
install.packages("randomForest")
install.packages("caret")
install.packages("pROC")
install.packages("corrplot")

#Carregar as bibliotecas a ser utilizadas

library(RWeka)
#para carregar a biblioteca RWeka é necessário o JAVA_HOME.como não possuo 
#foi decidido forçar a visualização pelo dplyr

library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(randomForest)
library(caret)
library(pROC) 
library(corrplot)

#carregar dataframes 

ds_arff <- read.csv("Acoustic_Extinguisher_Fire_Dataset.arff", header=FALSE, comment.char = "@")
View(ds_arff)

ds_txt <- read.csv("Acoustic_Extinguisher_Fire_Dataset_Citation_Request.txt", header=FALSE, comment.char = "@")
View(ds_txt)

#Aparentemente o arquivo arff se trata do dicionário de dados , mesmo arquivo TXT.


#ler o arquivo Xlsx puxando apenas a aba dos dados.
ds_xlsx <- read_excel("Acoustic_Extinguisher_Fire_Dataset.xlsx", sheet = "A_E_Fire_Dataset")
View(ds_xlsx)

ds<- ds_xlsx

#excluir as variaveis que não serao usadas
rm(ds_arff)
rm(ds_txt)
rm(ds_xlsx)

#Análise exploratória Inicial:
View(ds)
dim(ds)
str(ds)
names(ds)

#Verificar NA
colSums(is.na(ds)) 



#entender as variaveis


ds %>%
  count(SIZE)%>%
  arrange(desc(n))


ds %>%
  count(FUEL) %>%
  arrange(desc(n))

ds %>%
  count(DISTANCE)%>%
  arrange(desc(n)) 

ds %>%
  count(DESIBEL) %>%
  arrange(desc(n))%>%
  print(n=39)
  

ds %>%
  count(AIRFLOW) %>%
  arrange(desc(n))

ds %>%
  count(FREQUENCY) %>%
  arrange(desc(n))%>%
  print(n=55)

ds %>%
  count(STATUS) %>%
  arrange(desc(n))


#criar DataFrame com as informações contidas na documentação
# Tabela 1 - Informações para líquidos
liquid_info <- data.frame(
  FEATURES = c("SIZE", "FUEL", "DISTANCE", "DESIBEL", "AIRFLOW", "FREQUENCY", "STATUS"),
  MIN_MAX_VALUES = c("7, 12, 14, 16, 20", "Gasoline, Kerosene, Thinner", "10 - 190", "72 - 113", "0 - 17", "1-75", "0, 1"),
  UNIT_DESCRIPTIONS = c(
    "Recorded as 7 cm=1, 12 cm=2, 14 cm=3, 16 cm=4, 20 cm=5",
    "Fuel type", "", "", "", "", "0 indicates the non-extinction state, 1 indicates the extinction state"
  )
)


View(liquid_info)

# Tabela 2 - Informações para LPG
lpg_info <- data.frame(
  FEATURES = c("SIZE", "FUEL", "DISTANCE", "DESIBEL", "AIRFLOW", "FREQUENCY", "STATUS"),
  MIN_MAX_VALUES = c("Half throttle setting, Full throttle setting", "LPG", "10 - 190", "72 - 113", "0 - 17", "1-75", "0, 1"),
  UNIT_DESCRIPTIONS = c(
    "Half throttle = 6, Full throttle = 7",
    "Fuel type", "", "", "", "", "0 = non-extinction state, 1 = extinction state"
  )
)
View(lpg_info)

# Renomear colunas das tabelas para garantir consistência
colnames(liquid_info) <- colnames(lpg_info) <- c("FEATURES", "MIN_MAX_VALUES", "UNIT_DESCRIPTIONS")

# Combinar as duas tabelas em um único dataframe
dicionario_ds <- rbind(liquid_info, lpg_info)

# Remover linhas duplicadas para manter apenas registros únicos
dicionario_ds  <- dicionario_ds [!duplicated(dicionario_ds ), ]

# Exibir o novo dataframe combinado
View(dicionario_ds )

#excluir variaveis nao utilizadas
rm(liquid_info, lpg_info)

#engenharia de atributos

#1 - Converter o tipo de combustivel para representação numerica
# Criar fatores numerados para a coluna FUEL
ds$FUEL <- factor(ds$FUEL, levels = c("gasoline", "kerosene", "thinner", "lpg"))

# Substituir as referências por sua representação numérica
ds$FUEL <- as.numeric(ds$FUEL)

# Exibir o dataframe com as alterações
View(ds)



#Atualizar dicionario de dados:
dicionario_ds <- data.frame(
  FEATURES = c("SIZE", "FUEL", "DISTANCE", "DESIBEL", "AIRFLOW", "FREQUENCY", "STATUS"),
  MIN_MAX_VALUES = c("7, 12, 14, 16, 20,Half throttle, Full throttle  ", "Gasoline, Kerosene, Thinner, LPG", "10 - 190", "72 - 113", "0 - 17", "1-75", "0, 1"),
  UNIT_DESCRIPTIONS = c(
    "Recorded as 7 cm = 1, 12 cm = 2, 14 cm = 3, 16 cm = 4, 20 cm = 5, Half throttle = 6, Full throttle = 7 ",
    "Gasoline = 1, Kerosene = 2, Thinner = 3, LPG = 4", "", "", "", "", "0 = non-extinction state, 1 = extinction state"
  )
)
View(dicionario_ds)
str(ds)


#Separar Variaveis preditoras e target

var_preditoras <- c("SIZE","FUEL","DISTANCE","DESIBEL","AIRFLOW","FREQUENCY")
var_target <- c("STATUS")

#verificar correlacao das variaveis
matriz_corr <- cor(ds[, c(var_preditoras, var_target)])
print(matriz_corr)

# Criar o heatmap (mapa de calor) da matriz de correlação
ggplot(data = reshape2::melt(matriz_corr)) +
  geom_tile(aes(Var2, Var1, fill = value)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()

#Matriz aprimorada
corrplot(matriz_corr, method = "color", addCoef.col = "black", number.cex = 0.5, tl.cex = 0.7)

#verificar a quantidade de valeres unicos.
quantidade_valores_unicos_por_coluna <- numeric()

# Loop através de cada coluna e calcule a quantidade de valores únicos
for (coluna in names(ds)) {
  valores_unicos <- unique(ds[[coluna]])
  quantidade_valores_unicos <- length(valores_unicos)
  quantidade_valores_unicos_por_coluna <- c(quantidade_valores_unicos_por_coluna, quantidade_valores_unicos)
}

# Crie um novo dataframe com os resultados
resultado <- data.frame(Coluna = names(ds), Quantidade_Valores_Unicos = quantidade_valores_unicos_por_coluna)

# Exiba o novo dataframe
print(resultado)

###Transformar status como FATOR
ds$STATUS <- as.factor(ds$STATUS)

#separar ds_teste e ds_treino

# Divisão do conjunto de dados em treino e teste (exemplo usando 70% para treino e 30% para teste)
set.seed(123)  # Definir semente para reproduzibilidade
indices <- sample(2, nrow(ds), replace = TRUE, prob = c(0.7, 0.3))
conjunto_treino <- ds[indices == 1, ]
conjunto_teste <- ds[indices == 2, ]

# CRIE OS MODELOS



modelo_randomforest <- randomForest(STATUS ~ ., data = conjunto_treino, ntree = 100, nodesize=10)
print(modelo_randomforest)

# Faça previsões com o conjunto de teste
previsoes <- data.frame(observado = conjunto_teste$STATUS,
                        previsto = predict(modelo_randomforest, newdata = conjunto_teste))

View(previsoes)

# Avalie a acurácia do modelo (ou qualquer outra métrica de avaliação desejada)

class1 <- predict(modelo_randomforest, newdata = conjunto_teste, type = "prob")
class2 <- conjunto_teste$STATUS

pred <- prediction(class1[,2],class2)
perf <- performance(pred, "tpr","fpr")
plot (perf,col=rainbow(10))

confusionMatrix(previsoes$observado,previsoes$previsto)









