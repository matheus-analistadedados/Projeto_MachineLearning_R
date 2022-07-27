#_______________________________________________________________________________
#
#CAPÍTULO 15 - MACHINE LEARNING
#
#_______________________________________________________________________________

#instalando os pacotes
install.packages("Amelia")
install.packages("caret")
install.packages("randomForest")
installed.packages("ROSE")

# Carregando os pacotes 
library(Amelia)
library(caret)
library(ROSE)


#PASSO 1 - DEFINIÇÃO DO PROBLEMA
#Prever a inadimplência de clientes de cartão de crédito

#PASSO 2 - ANALISE E MODELAGEM DOS DADOS

#2.1 Ler o Arquivo Original

#2.2 Carregar o Arquivo:
#Definindo a pasta de trabalho -> função setwd ()
setwd("C:/Users/Matheus Lacerda/Desktop/Curso BI pra Ciencia de Dados/Cap15 - Machine Learnig") 
getwd() #informando o diretório

#carregar o arquivo
dados_clientes <- read.csv("dados/dataset.csv")

#2.3 Analisar o Arquivo
#Visualizar o arquivo
View(dados_clientes)

#Ver o Número de linhas e colunas
dim(dados_clientes)

#Ver a Estrutura dos Dadods
str(dados_clientes)

#Ver o Resumo
summary(dados_clientes)

#2.4 Limpar e Manipular os Dados

#2.4.1.	Verificação de Dados Faltantes
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados") #grafico

#2.4.2 Exclusão da primeira coluna ID
dados_clientes$ID <- NULL
#Analisar se o comando funcionou
dim(dados_clientes)
View(dados_clientes)

#2.4.3 Alteração do Título das Colunas 
#Ver o título das colunas
colnames (dados_clientes)
#Alterar os nomes das colunas pelo index da coluna
?colnames
colnames(dados_clientes)[1] <- "Limite_Cartao"
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)[24] <- "Inadimplente"
colnames (dados_clientes)

#2.4.4.	Alteração do Tipo de Dados das Colunas
#Ver o tipo das colunas
str (dados_clientes)
#Alterar tipo das colunas categóricas
# Genero
View(dados_clientes$Genero) 
str(dados_clientes$Genero) 
summary(dados_clientes$Genero)
#Apenas valores 1 e 2.
#Documentação: (1 = masculino; 2 = feminino)
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero, 
                             c(0,1,2), 
                             labels = c("Masculino",
                                        "Feminino"))
View(dados_clientes$Genero) 
str(dados_clientes$Genero) 
summary(dados_clientes$Genero) 

# Escolaridade
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade) 
#Foi observado que há números de 1 a 6. 
#Na documentação temos os valores correspondentes de 1 a 4 (1 = pós-graduação; 2 = universidade; 3 = ensino médio; 4 = outros)
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade, 
                                   c(0,1,2,3,4), 
                                   labels = c("Pos Graduado",
                                              "Graduado",
                                              "Ensino Medio",
                                              "Outros"))
#Verificar os dados ausentes após preenchimento da informação fornecida
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")
#Foram encontrados 345 dados faltantes, vou excluir estes dados neste caso. 
dados_clientes <- na.omit(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")

#Visualisar os resumos
View(dados_clientes$Escolaridade) 
str(dados_clientes$Escolaridade) 
summary(dados_clientes$Escolaridade)

# Estado Civil
str(dados_clientes$Estado_Civil) 
summary(dados_clientes$Estado_Civil)
#Foi observado que há números de 0 a 3.
#Documentação: (1 = casado; 2 = solteiro; 3 = outros)
#Analisando a frequência dos dados
data.frame(table(dados_clientes$Estado_Civil))
#valor 0 equivale a 54, como é um número pequeno vou tratar esses dados como categoria 3 outros

dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil, 
                                   c(-1,0,1,2,3),
                                   labels = c("Outro",
                                              "Casado",
                                              "Solteiro",
                                              "Outro"))

View(dados_clientes$Estado_Civil) 
str(dados_clientes$Estado_Civil) 
summary(dados_clientes$Estado_Civil)

# Idade
#Convertendo a variável para o tipo fator com faixa etária
str(dados_clientes$Idade) 
summary(dados_clientes$Idade) 
hist(dados_clientes$Idade)
dados_clientes$Idade <- cut(dados_clientes$Idade, 
                            c(0,30,50,100), 
                            labels = c("Jovem", 
                                       "Adulto", 
                                       "Idoso"))
View(dados_clientes$Idade) 
str(dados_clientes$Idade) 
summary(dados_clientes$Idade)
View(dados_clientes)

# PAY_0 a PAY_6
#Convertendo a variável que indica pagamentos para o tipo fator.
#Neste caso vou usar a função as.factor, pois não preciso alterar valores
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

#Visualisar o dataset após as modificações
str(dados_clientes)
View(dados_clientes)
summary(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x)))

#2.5. Variável dependente
#2.5.1.	Alteração do Tipo
dados_clientes$Inadimplente <- as.factor(dados_clientes$Inadimplente)

#2.5.2.	Analisando as Opções da Variável
#Conforme o meu caso tenho duas opções: Inadimplentes: 1 (Sim) e 2 (Não)
# Total de inadimplentes versus não-inadimplentes
?table
table(dados_clientes$Inadimplente)

#2.5.3.	Analisando as porcentagens
prop.table(table(dados_clientes$Inadimplente))

#2.5.4.	Dividir os Dados em Dados de Treinamento e Dados de Teste
#usar a função createDataPartition da biblioteca caret pra cirar o indice dos dados

indice <- createDataPartition(dados_clientes$Inadimplente, p = 0.75, list = FALSE)
dim(indice)
#Criando os dados de treinamento e teste
dados_treinamento <- dados_clientes[indice, ]
dados_teste <- dados_clientes [-indice, ]

#Número de registros nos dataset
dim(dados_treinamento)
dim(dados_teste)
dim(dados_clientes)

# Veja as porcentagens entre as classes
prop.table(table(dados_treinamento$Inadimplente))
prop.table(table(dados_teste$Inadimplente))
prop.table(table(dados_clientes$Inadimplente))

# Comparando as porcentagens entre as classes de treinamento, teste e dados originais
comparar_dados <- cbind(prop.table(table(dados_treinamento$Inadimplente)), 
                       prop.table(table(dados_teste$Inadimplente)),
                       prop.table(table(dados_clientes$Inadimplente)))
colnames(comparar_dados) <- c("Treinamento", "Teste", "Original")
comparar_dados




#PASSO 3 - MODELO DE MACHINE LEARNING

#3.1.	Balancear os Dados
#Neste Caso irei criar dados sintéticos pra aumentar os dados 
#VIsualisar os dados
table(dados_treinamento$Inadimplente)
prop.table(table(dados_treinamento$Inadimplente))

#Criando os dados sintéticos com a função ovun.sample método "Over" Igualar a quantidade menor na maior

?ovun.sample
dados_treinamento_bal<-ovun.sample(Inadimplente ~ ., data=dados_treinamento, method="over")$data
table(dados_treinamento_bal$Inadimplente)
prop.table(table(dados_treinamento_bal$Inadimplente))

#3.2.	Criar a primeira versão e testá-la
#A primeira Versão será Árvore de Decisão

modelo_v1 <- randomForest(Inadimplente ~ ., data = dados_treinamento_bal)
modelo_v1

#Previsões com dados de teste
?predict
previsoes_v1 <- predict(modelo_v1, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$Inadimplente, positive = "1")
cm_v1

# Calculando Precision, Recall e F1-Score, métricas de avaliação do modelo preditivo
y <- dados_teste$Inadimplente
y_pred_v1 <- previsoes_v1

?posPredValue
precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1


#3.3.    Criar as demais versões caso necessário e testá-las

# Importância das variáveis preditoras para as previsões
?varImpPlot
varImpPlot(modelo_v1)
#Neste caso as variáveis Genero, Estado Civil, Idade, Escolaridade, PAY_6, PAY5, PAY_4 e PAY3 tem menos importância.

#_______ Funções pra ver o resultado anterior em forma de gráfico de barras_______

# Obtendo as variáveis mais importantes
imp_var <- importance(modelo_v1)
varImportance <- data.frame(Variables = row.names(imp_var), 
                            Importance = round(imp_var[ ,'MeanDecreaseGini'],2))

# Criando o rank de variáveis baseado na importância
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Usando ggplot2 para visualizar a importância relativa das variáveis
ggplot(rankImportance, 
       aes(x = reorder(Variables, Importance), 
           y = Importance, 
           fill = Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), 
            hjust = 0, 
            vjust = 0.55, 
            size = 4, 
            colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 

#___________________________________________________________________________________

# Construindo a segunda versão do modelo apenas com as variáveis mais importantes
dados_treinamento_imp <- dados_treinamento_bal
dados_treinamento_imp$Genero <- NULL
dados_treinamento_imp$Estado_Civil <- NULL
dados_treinamento_imp$Idade <- NULL
dados_treinamento_imp$Escolaridade <- NULL
dados_treinamento_imp$PAY_6 <- NULL
dados_treinamento_imp$PAY_5 <- NULL
dados_treinamento_imp$PAY_4 <- NULL
dados_treinamento_imp$PAY_3 <- NULL
colnames(dados_treinamento_imp)

modelo_v2 <- randomForest(Inadimplente ~ ., data = dados_treinamento_imp)
modelo_v2

#AVALIANDO
#Previsões com dados de teste
?predict
previsoes_v2 <- predict(modelo_v2, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$Inadimplente, positive = "1")
cm_v2
cm_v1

# Calculando Precision, Recall e F1-Score, métricas de avaliação do modelo preditivo
y <- dados_teste$Inadimplente
y_pred_v2 <- previsoes_v2

?posPredValue
precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1
head(dados_treinamento_imp)

#3.4. Salvando o modelo em disco

saveRDS(modelo_v2, file = "modelo_v2.rds")

##################################

#4. Testar o modelo

#4.1.  Carregando o arquivo
modelo_final <- read.csv("modelo_v2.rds")

#4.2. Testar o modelo
#Previsões com novos dados de 3 clientes

# Dados dos clientes
Limite_Cartao <- c(35000, 22000, 70000)
PAY_0 <- c(0, 0, 0)
PAY_2 <- c(0, 0, 0) 
BILL_AMT1 <- c(35000, 4200, 13000)
BILL_AMT2 <- c(18000, 5200, 8000)
BILL_AMT3 <- c(28000, 2700, 10000)
BILL_AMT4 <- c(22000, 6300, 6200)
BILL_AMT5 <- c(16000, 5400, 8000)
BILL_AMT6 <- c(14000, 1200, 8700)
PAY_AMT1 <- c(1100, 1000, 1200) 
PAY_AMT2 <- c(1500, 1300, 1150)
PAY_AMT3 <- c(2100, 2500, 19200) 
PAY_AMT4 <- c(1800, 1100, 1900) 
PAY_AMT5 <- c(2900, 1000, 3000) 
PAY_AMT6 <- c(1300, 2500, 1400) 

# Concatena em um dataframe
novos_clientes <- data.frame(Limite_Cartao, PAY_0, PAY_2, BILL_AMT1, BILL_AMT2, BILL_AMT3, BILL_AMT4, BILL_AMT5, BILL_AMT6,
                             PAY_AMT1, PAY_AMT2, PAY_AMT3, PAY_AMT4, PAY_AMT5, PAY_AMT6)
View(novos_clientes)

# Checando os tipos de dados
str(dados_treinamento_imp)
str(novos_clientes)

# Convertendo os tipos de dados
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treinamento_imp$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treinamento_imp$PAY_2))

str(novos_clientes)

# Previsões
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)
View(previsoes_novos_clientes)





