##### Analise dos dados #####
data = read.table("noshowappointments/KaggleV2-May-2016.csv",
                  header=T, sep=",")
head(data)
attach(data)

#Dados de distancia
data$ScheduledDay
data$AppointmentDay

agenda = gsub("T"," ",ScheduledDay)
agenda = gsub("Z","",agenda)
class(agenda)

agenda = strptime(agenda, "%Y-%m-%d %H:%M:%S")
agenda

dia = gsub("T"," ",AppointmentDay)
dia = gsub("Z","",dia)

dia = strptime(dia, "%Y-%m-%d %H:%M:%S")
dia

tempo = difftime(dia,agenda,units = "day")
tempo = as.integer(tempo)

head(data)
data = cbind(data,tempo)
head(data)

#Amostras com erro 
data$ScheduledDay[which(tempo<0)] = NA
data$AppointmentDay[which(tempo<0)] = NA


##### Regressao Logistica #####

glm.fits = glm(No.show ~ Age + Alcoholism + Diabetes + Gender + Handcap + Hipertension + Neighbourhood
                + Scholarship + SMS_received + tempo,
                data = data, 
                family = binomial)

summary(glm.fits)

#Mantendo so as variaveis com alguma significancia

glm.fits = glm(No.show ~ Age + Alcoholism + Diabetes + Hipertension + Scholarship + SMS_received + tempo,
                data = data, 
                family = binomial)

summary(glm.fits)

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





