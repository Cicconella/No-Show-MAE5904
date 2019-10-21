# PROJETO NO-SHOW 
library(ggplot2)
library(gridExtra)
library(lubridate)
library(dplyr)



##### Carrega Dados #####
<<<<<<< HEAD
data <- read.table("noshowappointments/KaggleV2-May-2016.csv",
                   header=T, sep=",")
head(data )
attach(data )
=======
data = read.table("noshowappointments/KaggleV2-May-2016.csv",
                   header=T, sep=",")
head(data)
attach(data)
>>>>>>> 1a82921f53164758f29d671ea56e38bd97cd7981

str(data)
names(data)

data$Gender <- factor(data$Gender, levels = c("M", "F"))
data$AppointmentDay <- as.POSIXct(strptime(data$AppointmentDay, "%Y-%m-%dT%H:%M:%SZ"))
data$ScheduledDay   <- as.POSIXct(strptime(data$ScheduledDay, "%Y-%m-%dT%H:%M:%SZ"))
data$Diabetes <- as.logical(data$Diabetes)
data$Alcoholism <- as.logical(data$Alcoholism)
data$Hipertension <- as.logical(data$Hipertension)
data$Handcap <- factor(data$Handcap)
data$Neighbourhood <- factor(data$Neighbourhood)
data$Scholarship <- as.logical(data$Scholarship)
data$SMS_received <- as.logical(data$SMS_received)
data$No.show <- factor(data$No.show, levels = c("Yes", "No"))

summary(data)

##### Analise da variavel no-show #####

# estudo para a variavel resposta no-show
status_table <- table(data$No.show)
status_table

ggplot(data, aes(x=No.show, fill=No.show)) + geom_bar() + scale_fill_manual(values=c("grey60", "#723881"))

#25.3% das pessoas nao compareceu para consulta comparando com os que compareceu
(status_table["Yes"]/status_table["No"])*100

#20.2% das pessoas nao compareceu em relacao ao total de agendados
(status_table["Yes"]/(status_table["No"]+status_table["Yes"]))*100

# numero total de paciente distintos, isso quer dizer que tem pacientes que marcar mais de uma vez
length(unique(data$PatientId))

##### Genero #####

# add no-show e gender
tab_Gender <- table(data$Gender, data$No.show)
addmargins(tab_Gender)

# embora o numero de feminino eh quase dobro do masculino, a proporcao de no.show entre os sexos parecidos
prop.table(tab_Gender,1)

g_Gender_1 <- ggplot(data, aes(x=Gender, fill=Gender)) + geom_bar(position="dodge")
g_Gender_2 <- ggplot(data, aes(x=Gender, fill=No.show)) + geom_bar(position="fill") + scale_fill_manual(values=c("grey50", "#723881"))
grid.arrange(g_Gender_1, g_Gender_2,ncol=2, top='Gender distribution')

##### Age #####

table(Age)

Age[which(Age<0)] = NA

barplot(table(Age))

faixas = c("0-5","6-18","19-60","61+")

bebe = which(Age<6)
crianca = which(Age>5 & Age<19)
adulto = which(Age>18 & Age<61)
idoso = which(Age>60)

barplot(c(length(bebe), length(crianca), length(adulto),length(idoso))/length(Age), 
        names = faixas)

faixa_etaria = rep(NA, length(Age))
faixa_etaria[bebe] = "Bebe"
faixa_etaria[crianca] = "Criança"
faixa_etaria[adulto] = "Adulto"
faixa_etaria[idoso] = "Idoso"

barplot(table(No.show,faixa_etaria), beside = T)
barplot(t(t(table(No.show,faixa_etaria))/apply(table(No.show,faixa_etaria),2,sum)), beside = T)


##### SMS #####

# add no-show e sms_received
tab_Sms <- table(data$SMS_received, data$No.show)
addmargins(tab_Sms)

# os que receberam a mensagem, 72% compareceu, os que nao receberam a mensagem, 83% compareceu, ou seja, nao ha muito efeito sobre a mensagem
prop.table(tab_Sms,1)


##### 

# add no-show e (Diabetes, Alcoholism, Hipertension, Handcap, Scholarship, SMS_received)
g_Diabetes <- ggplot(data, aes(x=Diabetes, fill=No.show)) + geom_bar(position="fill") + theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881"))
g_Alcoholism <- ggplot(data, aes(x=Alcoholism, fill=No.show)) + geom_bar(position="fill") + theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881"))
g_Hipertension <- ggplot(data, aes(x=Hipertension, fill=No.show)) + geom_bar(position="fill") + scale_fill_manual(values=c("grey50", "#723881"))
g_Handcap <- ggplot(data, aes(x=Handcap, fill=No.show)) + geom_bar(position="fill") + theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881"))
g_Scholarship <- ggplot(data, aes(x=Scholarship, fill=No.show)) + geom_bar(position="fill") + theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881"))
g_SMS_received <- ggplot(data, aes(x=SMS_received, fill=No.show)) + geom_bar(position="fill") + scale_fill_manual(values=c("grey50", "#723881"))

# Alcoholism eh praticamente igual, isso quer dizer que nao tem efeito no variavel resposta
# Diabetes, Hipertension, Scholarship, Handcap tem pequenas diferencas que nao sao relevantes
# SMS_received = quem que recebe mensagem tem mais proporcao que nao compareceu, ou seja, mensagem nao faz efeito
grid.arrange(g_Diabetes, g_Alcoholism, g_Hipertension, ncol=3, top='Variables effect') 
grid.arrange(g_Handcap, g_Scholarship, g_SMS_received, ncol=3, top='Variables effect')

#add no-show e Neighbourhood
g_neigh_amount <- ggplot(data,aes(x=Neighbourhood,fill=No.show))+
  geom_bar(position="stack")+
  scale_fill_manual(values=c("grey50","#723881"))+
  theme(axis.text.x = element_text(angle=45, hjust=1,size=5))+
  ggtitle("Amount of No Shows across Neighbourhood")

g_neigh_prop <- ggplot(data,aes(x=Neighbourhood,fill=No.show))+
  geom_bar(position="fill")+
  scale_fill_manual(values=c("grey50","#723881"))+
  theme(axis.text.x = element_text(angle=45, hjust=1,size=5))+
  ggtitle("Proportion of No Shows across Neighbourhood")

# se temos mais informacoes como a distancia entre o lugar da consulta e o endereco do paciente, vai sair mais informacoes,
# tem alguns bairros tem poucas informacoes, portanto nao da para entrar em consideracao
# tem que ver se esse endereco eh do hospital? 
grid.arrange(g_neigh_amount, g_neigh_prop, nrow=2, top='Neighbourhood Variable effect')


# add no-show e age
densAge <- ggplot(data,aes(x=Age,fill=No.show))+
  geom_density(color="white",alpha=0.4)+
  ggtitle("Density of Age across No Show")+
  scale_fill_manual(values=c("grey50","#723881"))
densAge
# density = count/n
# n eh o numero de no-show (yes ou no)
# densidade = freq.relativa / intervalo
p <- ggplot_build(densAge) 
head(p$data[[1]], 3)

g_Age_1 <- ggplot(data, aes(x=Age,fill=No.show)) + geom_bar(position = "stack") + theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881"))
g_Age_2 <- ggplot(data, aes(x=No.show, y=Age, fill=No.show)) + geom_boxplot() + scale_fill_manual(values=c("grey50", "#723881"))
grid.arrange(g_Age_1, g_Age_2,ncol=2, top='Age Variable effect') 

# observamos que temos idade de -1, que sao pacientes gravidas, elas frequentam mais nas consultas
# pelo boxplot, tem uma concentracao maior no intervalo entre 15 ate 60, ou seja, tem mais jovem e adultos do que idosos, 
# e tem dois outlier de 115 anos
summary(data$Age)


# add no-show e Appointment
# tem somente agendamentos desde 2016-04-29 ate 2016-06-08
ggplot(data, aes(x=AppointmentDay, fill=No.show)) + geom_bar(position="stack") + 
  scale_fill_manual(values=c("grey50", "#723881")) + ggtitle("Year 2016")
# + scale_x_datetime(format("%Y-%m-%d"))

# nao houve muita diferenca entre os dias de agendamento
ggplot(data, aes(x=AppointmentDay, fill=No.show)) + geom_bar(position="fill") + 
  scale_fill_manual(values=c("grey50", "#723881")) + ggtitle("Year 2016")


# add no-show e ScheduledDay 
# podemos ver que a tendencia de horario de consulta eh de manha e a tarde, e de manha eh mais 
# e o comportamento de status sao parecidos
ggplot(data, aes(x=hour(ScheduledDay), fill=No.show)) + geom_density() + 
  facet_grid(.~No.show) + scale_fill_manual(values=c("grey50", "#723881"))

ggplot(data, aes(x=hour(ScheduledDay), fill=No.show)) + geom_bar(position="stack") + 
  scale_fill_manual(values=c("grey50", "#723881")) 

# mais tarde o horario do dia tem a tendencia de aumentar o numero de Noshow
ggplot(data, aes(x=hour(ScheduledDay), fill=No.show)) + geom_bar(position="fill") + 
  scale_fill_manual(values=c("grey50", "#723881")) 

ggplot(data, aes(x=hour(ScheduledDay), fill=No.show)) + geom_bar(position="fill") + 
  scale_fill_manual(values=c("grey50", "#723881")) 

#calcular lag entre as duas datas
awaitingDays = difftime(data$AppointmentDay, data$ScheduledDay, units="days")
min(awaitingDays) #Time difference of -6.575926 days
max(awaitingDays) #Time difference of 178.5965 days

g_AwaitingTime_1 <- ggplot(data, aes(x=No.show, y=awaitingDays, fill=No.show)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("grey50", "#723881"))

g_AwaitingTime_2 <- ggplot(data, aes(x=awaitingDays, fill=No.show)) + 
  geom_density(alpha=0.4) + 
  coord_cartesian(xlim=c(0, 200)) +
  scale_fill_manual(values=c("grey50", "#723881")) 

# as pessoas que comparecem na consulta tem menos dias de espera, mas tem muitos outliers
# e precisamos tirar fora os que tem awaiting negativos
grid.arrange(g_AwaitingTime_1, g_AwaitingTime_2,ncol=2, top='awaitingDays distribution')


# - calcular day of the week of the variable scheduleday

# data$DayOfTheWeek <- factor(data$DayOfTheWeek, 
#                             levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday" , 
#                                      "Saturday", "Sunday"))


##### Analise do tempo de espera entre o agendamento e consulta #####
dados$ScheduledDay
dados$AppointmentDay

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
dados$ScheduledDay[which(tempo<0)] = NA
dados$AppointmentDay[which(tempo<0)] = NA

#Distribuicao do tempo
boxplot(tempo~No.show)
hist(tempo,freq = F, col="green", nc=30)


