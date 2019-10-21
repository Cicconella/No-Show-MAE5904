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

#Amostras com erro 
data$ScheduledDay[which(tempo<0)] = NA
data$AppointmentDay[which(tempo<0)] = NA


##### Regressao Logistica #####

glm.fits <- glm(No.show ~ Age + Alcoholism + Diabetes + Gender + Handcap + Hipertension + Neighbourhood
                + Scholarship + SMS_received + tempo,
                data = data, 
                family = binomial)

summary(glm.fits)

#Mantendo so as variaveis com alguma significancia

glm.fits <- glm(No.show ~ Age + Alcoholism + Diabetes + Hipertension + Scholarship + SMS_received + tempo,
                data = data, 
                family = binomial)

summary(glm.fits)

#Hipertesao nao foi significativo
glm.fits <- glm(No.show ~ Age + Alcoholism + Diabetes + Scholarship + SMS_received + tempo,
                data = data, 
                family = binomial)

summary(glm.fits)

coef(glm.fits)

glm.probs <- predict(glm.fits, type="response")
summary(glm.probs)

levels(No.show)
contrasts(No.show)

glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")


#Resultado muito muito ruim para o Yes
table(glm.pred, No.show)
mean(glm.pred == No.show)



