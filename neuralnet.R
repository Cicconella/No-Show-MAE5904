dados = read.table("dados_no_show.csv", header=T, sep=",", stringsAsFactors = T)
head(dados)
dim(dados)
#summary(dados)
#attach(dados)
library(nnet)
library(caret)

################# nnet

?nnet

model_nnet <- nnet(formula = No.show ~ Gender + Age + Neighbourhood + Scholarship +
                     Hipertension + Diabetes + Alcoholism + Handcap + SMS_received +
                     Hour + Day_Week + Day_Week_Appointment + Wait, 
                   data=dados, size=4, decay=0.0001, maxit = 700, 
                   trace= FALSE)

head(dados)

#summary(model_nnet)
x = dados[,c(3:11,13:16)]
y = dados[,12]

pred <- predict(model_nnet,x,type="class") 
resultado <- confusionMatrix(as.factor(pred), y)
resultado

#### Dados de treinamento ####

dados = read.table("treinamento.csv", header=T, sep=",", stringsAsFactors = T)
head(dados)
dim(dados)
summary(dados)
attach(dados)


model_nnet <- nnet(formula = No.show ~ Gender + Age + Neighbourhood + Scholarship +
                     Hipertension + Diabetes + Alcoholism + Handcap + SMS_received +
                     Hour + Day_Week + Day_Week_Appointment + Wait, 
                   data=dados, size=4, decay=0.0001, maxit = 700, 
                   trace= FALSE)

head(dados)
#summary(model_nnet)
x = dados[,c(3:15)]
y = dados[,16]

pred <- predict(model_nnet,x,type="class") 
resultado <- confusionMatrix(as.factor(pred), y)
resultado

#### Dados de teste ####

dados = read.table("teste.csv", header=T, sep=",", stringsAsFactors = T)
head(dados)
summary(dados)

require(gdata)
head(dados)
#summary(model_nnet)
dados = dados[-which(dados$Neighbourhood=="PARQUE INDUSTRIAL"),]
dados$Neighbourhood = drop.levels(dados$Neighbourhood)

unique(dados$Neighbourhood)
x = dados[,c(3:11,13:16)]
y = dados[,12]

pred <- predict(model_nnet,x,type="class") 
resultado <- confusionMatrix(as.factor(pred), y)
resultado



# #install.packages("neuralnet")
# library(neuralnet)
# 
# dados = read.table("treinamento.csv", header=T, sep=",", stringsAsFactors = T)
# head(dados)
# summary(dados)
# attach(dados)
# 
# dados$No.show 
# dados$No.show [which(dados$No.show =="Yes")] = 1 
# dados$No.show [which(dados$No.show =="No")] = 0 
# dados$No.show = as.numeric(dados$No.show)
# 
# dados$Gender[which(dados$Gender=="F")] = 0
# dados$Gender[which(dados$Gender=="M")] = 1
# dados$Gender = as.numeric(dados$Gender)
# dados$Gender
# 
# dados$Gender = as.numeric(dados$Gender) 
# dados$Age = as.numeric(dados$Age) 
# dados$Scholarship = as.numeric(dados$Scholarship) 
# 
# # +Age+Neighbourhood+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap 
# # +SMS_received+Wait+Hour+Day_Week + Day_Week_Appointment
# 
# model_nn <- neuralnet(formula = No.show ~ Gender+Age+Scholarship,
#                       data=dados, hidden=1, rep=1, err.fct = "ce", linear.output = FALSE)
# model_nn
# 
# final_output=cbind (dados$No.show, 
#                     as.data.frame(model_nn$net.result) )
# print(final_output)



