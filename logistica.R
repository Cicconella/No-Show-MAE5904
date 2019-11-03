#####################   Ajuste Modelo Logistico   ######################

# Carregando as Bibliotecas
library(mlbench)
library(caret)
library(glmnet)
library(e1071)
library(MASS)
library(mlbench)


# Leitura e tratamento final nos dados
dados = read.table("treinamento.csv", header=T, sep=",")
summary(dados)
str(dados)
dados <- dados[,-c(1,2)]
dados = dados[-which(is.na(dados$Wait)),]
summary(dados)

dados_teste = read.table("teste.csv", header=T, sep=",")
summary(dados_teste)
str(dados_teste)
dados_teste <- dados_teste[,-c(1,2)]
dados_teste <- dados_teste[-which(dados_teste$Neighbourhood=='PARQUE INDUSTRIAL'),]
summary(dados_teste)

# Treinamento do Modelo Logistico
control <- trainControl(method="repeatedcv", number=10)# Parametro de Cross Validation para treinamento
modelLogistica <- train( No.show ~ .,data = dados,trControl = control,method = "glm",family=binomial, na.action = na.omit)# train the model on training set

#Métricas de Ajuste do Modelo
resultados <- predict(modelLogistica, newdata = dados,type = "raw")
confusionMatrix(as.factor(resultados), as.factor(dados$No.show))
summary(modelLogistica)

#Aplicação do Modelo na Base de Teste
resultados_teste <- predict(modelLogistica, newdata=dados_teste, type="raw", na.action = na.omit)
confusionMatrix(as.factor(resultados_teste), as.factor(dados_teste$No.show)) # Métricas de ajuste na base de Teste






#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# Rascunho
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#Carregando Pacotes necessários
library(caret)
library(MASS)
library(tidyverse)

dados = read.table("treinamento.csv", header=T, sep=",")
head(dados)
summary(dados)
dim(dados)

dados = dados[-which(is.na(dados$Wait)),]
dim(dados) # Verificando quantas linhas foram removidas ( apenas 2)

#### Analise Regressão Logistica ####

glm.fits = glm(No.show ~ Gender+Age+Neighbourhood+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap
               +SMS_received+Wait+Hour+Day_Week + Day_Week_Appointment,
               data = dados, 
               family = binomial)

summary(glm.fits)


resultados = predict(glm.fits, type = "response")
#resultados = predict(glm.fits, type= "terms")
resultados
par(mfrow=c(1,2))
hist(resultados)
t=0.3
length(resultados)
resultados[resultados<t] = "No"
resultados[resultados!="No"] = "Yes"

length(resultados)
length(dados$No.show)

table(resultados)
table(resultados==dados$No.show)
mean(resultados==dados$No.show)


# Sensibilidade e Especificidade (Usando funções do pacote Caret)
conf_matrix<-table(as.factor(resultados), as.factor(dados$No.show))
conf_matrix
sensitivity(conf_matrix)
specificity(conf_matrix)

#Variable Selection
step.model <- glm.fits %>% stepAIC(trace = FALSE)
coef(step.model)
summary(step.model)

# Sensibilidade e Especificidade Modelo Reduzido
resultados = predict(step.model, type = "response")
resultados[resultados<t] = "No"
resultados[resultados!="No"] = "Yes"
conf_matrix<-table(as.factor(resultados), as.factor(dados$No.show))
conf_matrix
sensitivity(conf_matrix)
specificity(conf_matrix)


#### Acerto = 78%

teste = read.table("teste.csv", header=T, sep=",")
head(teste)
dim(teste)

resultados = predict(glm.fits, teste,  type = "response") # Inclui o tipo de resposta neste caso
length(resultados)
hist(resultados)
t=0.5
length(resultados)
resultados[resultados<t] = "No"
resultados[resultados!="No"] = "Yes"

length(resultados)
length(teste$No.show)

table(resultados,teste$No.show)
mean(resultados==teste$No.show)


# Sensibilidade e Especificidade (Usando funções do pacote Caret)
conf_matrix<-table(as.factor(resultados), as.factor(teste$No.show))
conf_matrix
sensitivity(conf_matrix)
specificity(conf_matrix)


#### Acerto = 79%

#Mantendo so as variaveis com alguma significancia

glm.fits = glm(No.show ~ Age + Scholarship+ Hipertension + Diabetes+ Alcoholism + SMS_received + Wait + Hour 
               + Day_Week + Day_Week_Appointment,
                data = dados, 
                family = binomial)

summary(glm.fits)

resultados = predict(glm.fits, teste)
length(resultados)

t=0.5
length(resultados)
resultados[resultados<t] = "No"
resultados[resultados!="No"] = "Yes"

length(resultados)
length(teste$No.show)

table(resultados,teste$No.show)
mean(resultados==teste$No.show)

#Hipertesao nao foi significativo
glm.fits = glm(No.show ~ Age + Alcoholism + Diabetes + Scholarship + SMS_received + tempo,
                data = dados, 
                family = binomial)

summary(glm.fits)

coef(glm.fits)

glm.probs = predict(glm.fits, type="response")
summary(glm.probs)

levels(No.show)
contrasts(No.show)

glm.pred = ifelse(glm.probs > 0.5, "Yes", "No")


#Resultado muito muito ruim para o Yes
table(glm.pred, No.show)
mean(glm.pred == No.show)

#separando os dados em conjunto de treino e teste
dim(data)
train = sample(seq(1:dim(data)[1]))
train = train[1:(dim(data)[1]*0.8)]
length(train)

teste = cbind(data[-train, c("No.show","Age","Alcoholism","Diabetes","Scholarship","SMS_received","tempo")], 
              teste[-train])
dim(teste)

teste_no_show = data$No.show[-train]
dim(teste_no_show)

glm.fit2 = glm(No.show ~ Age + Alcoholism + Diabetes + Scholarship + SMS_received + tempo,
               data = data, 
               family = binomial, subset = train)
glm.probs2 = predict(glm.fit2, teste, type="response")
glm.pred2 = ifelse(glm.probs2 > 0.5, "Yes", "No")
glm.pred2

#Teste
table(glm.pred2, teste_no_show)

teste_no_show = data$No.show[-train]





