dados = read.table("treinamento.csv", header=T, sep=",")
head(dados)
summary(dados)

#### Analise Regressão Linear ####

dados$No.show = as.character(dados$No.show)
dados$No.show[dados$No.show=="Yes"] = 1
dados$No.show[dados$No.show=="No"] = 0
dados$No.show = as.numeric(dados$No.show)
dim(dados)
length(No.show)

mod = lm(No.show~Gender+Age+Neighbourhood+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap
         +SMS_received+Wait+Hour+Day_Week, data=dados)
mod
summary(mod)
names(resultados)
length(fitted(mod))

resultados = predict(mod, dados)
t=0.5
length(resultados)
resultados[resultados<t] = 0
resultados[resultados>t] = 1

length(resultados)
length(dados$No.show)

table(resultados,dados$No.show)
table(resultados==dados$No.show)

table(resultados==dados$No.show)[2]/sum(table(resultados==dados$No.show))

#### Acerto = 80%

teste = read.table("teste.csv", header=T, sep=",")
head(teste)
dim(teste)

teste$No.show = as.character(teste$No.show)
teste$No.show[teste$No.show=="Yes"] = 1
teste$No.show[teste$No.show=="No"] = 0
teste$No.show = as.numeric(teste$No.show)
dim(teste)
length(teste$No.show)

resultados = predict(mod, teste)
length(resultados)

t=0.5
length(resultados)
resultados[resultados<t] = 0
resultados[resultados>t] = 1

length(resultados)
length(teste$No.show)

table(resultados,teste$No.show)
table(resultados==teste$No.show)

table(resultados==teste$No.show)[2]/sum(table(resultados==teste$No.show))

#### Acerto = 79%

