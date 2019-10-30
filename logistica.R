dados = read.table("treinamento.csv", header=T, sep=",")
head(dados)
summary(dados)

dados = dados[-which(is.na(dados$Wait)),]

#### Analise Regressão Logistica ####

glm.fits = glm(No.show ~ Gender+Age+Neighbourhood+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap
               +SMS_received+Wait+Hour+Day_Week + Day_Week_Appointment,
               data = dados, 
               family = binomial)

summary(glm.fits)


resultados = predict(glm.fits, type = "response")
resultados
t=0.5
length(resultados)
resultados[resultados<t] = "No"
resultados[resultados!="No"] = "Yes"

length(resultados)
length(dados$No.show)

table(resultados)
table(resultados==dados$No.show)
mean(resultados==dados$No.show)

#### Acerto = 80%

teste = read.table("teste.csv", header=T, sep=",")
head(teste)
dim(teste)

teste = teste[-which(is.na(teste$Wait)),]
length(teste$No.show)

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

#### Acerto = 79%

#Mantendo so as variaveis com alguma significancia

glm.fits = glm(No.show ~ Age + Alcoholism + Diabetes + Hipertension + Scholarship + SMS_received + Wait,
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
                data = data, 
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





