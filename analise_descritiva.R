# PROJETO NO-SHOW 
library(ggplot2)
library(gridExtra)
library(lubridate)
library(dplyr)
library(xtable)
library(reshape2) 
require(plyr)


##### Carrega Dados #####
data <- read.table("noshowappointments/KaggleV2-May-2016.csv",
                   header=T, sep=",")

head(data )
attach(data )

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
    status_table <- round(table(data$No.show)/length(data$No.show),3)

G1 <-  ggplot(data) + 
  theme_bw() + 
  geom_bar(mapping = aes(x = No.show, y = ..prop.., group = 1),fill = "#723881", stat = "count",colour="black") + 
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete( labels = c("Yes" = "Sim","No" = "Não")) +
  theme(text = element_text(size=15)) +
  ylab("Frequência Relativa") +
  xlab("No Show")


pdf("GNoShow.pdf", width = 8, height = 8, paper = "a4r") ; G1; dev.off()




  #25.3% das pessoas nao compareceu para consulta comparando com os que compareceu
  (status_table["Yes"]/status_table["No"])*100
  
  #20.2% das pessoas nao compareceu em relacao ao total de agendados
  (status_table["Yes"]/(status_table["No"]+status_table["Yes"]))*100
  
  # numero total de paciente distintos, isso quer dizer que tem pacientes que marcar mais de uma vez
    length(unique(data$PatientId))
  
#Freq. da Quantidades de consultas
Freq_consul <- count(data$PatientId)

consult1  <- length(Freq_consul[Freq_consul$freq==1,2])
consult2  <- length(Freq_consul[Freq_consul$freq==2,2])
consult3 <- length(Freq_consul[Freq_consul$freq==3,2])
consult10plus  <- length(Freq_consul[Freq_consul$freq>3,2])

consultas <- c("1","2","3","3+")

data.consultas <- data.frame(x = c(rep("1",consult1),rep("2",consult2),
                                 rep("3",consult3),rep("3+",consult10plus)))

data.consultas$x <- factor(data.consultas$x,levels = c("1", "2","3", "3+"))

porcent.data <- data.frame(x = c("1", "2","3", "3+"), y = round(c(consult1,consult2,
                                            consult3,consult10plus)/length(Freq_consul$freq),2))

ConsulA <- ggplot(data.consultas) + 
              theme_bw() + 
              geom_bar(aes(x=x, y = ..prop..,  group = 1), fill = "#723881", stat = "count",colour="black",position="dodge")+
              geom_text(data=porcent.data, aes(x = x, y = y+0.02,
                                  label = paste0(y*100,"%")), size=6) +
              scale_y_continuous(labels = scales::percent_format()) +
              theme(text = element_text(size=18)) +
              ylab("Frequência Relativa")+
              xlab("Quantidade de agendamentos")


ConsulB <- ggplot(Freq_consul, aes(x=freq)) + theme_bw() +
              ylab("Frequência") + xlab("Quantidade de agendamentos") + 
              geom_bar(position = "stack",colour = "black", fill = "#723881") +
              theme(legend.position = "none") + 
              scale_x_continuous(breaks=c(1, 15, 30, 50, 88)) +
              theme(text = element_text(size=18)) 
          

pdf("Gconsultas.pdf", width = 20, height = 20) ;grid.arrange(ConsulB, ConsulA,ncol=2); dev.off()


#freq. agendamento vs no show
filterdata1 <- filter(data, PatientId %in% Freq_consul[Freq_consul$freq == 1,1])
filterdata2 <- filter(data, PatientId %in% Freq_consul[Freq_consul$freq == 2,1])
filterdata3 <- filter(data, PatientId %in% Freq_consul[Freq_consul$freq == 3,1])
filterdata3plus <- filter(data, PatientId %in% Freq_consul[Freq_consul$freq > 3,1])


NAgenda <- cbind(table(filterdata1$No.show), table(filterdata2$No.show), table(filterdata3$No.show),
            table(filterdata3plus$No.show))

colnames(NAgenda) <- c("1","2","3","3+")
addmargins(NAgenda)

xtable(prop.table(NAgenda,2)*100,digits = 2)


##### Genero #####

# add no-show e gender
tab_Gender <- table(data$Gender, data$No.show)
addmargins(tab_Gender)

# embora o numero de feminino eh quase dobro do masculino, a proporcao de no.show entre os sexos parecidos
xtable(prop.table(tab_Gender,1)*100,digits = 2)

g_Gender_1 <- ggplot(data) + 
  theme_bw() + 
  scale_x_discrete( labels = c("F" = "Feminino","M" = "Masculino")) +
  geom_bar(aes(x=Gender, y = ..prop..,  group = 1), fill = "#723881", stat = "count",colour="black",position="dodge")+
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Frequência Relativa")+
  theme(text = element_text(size=20)) +
  xlab("Gênero")


data.Gender.No.show <- data.frame(melt(prop.table(tab_Gender,1),id.vars="Yes"))

g_Gender_2 <-   ggplot(data.Gender.No.show, aes(x = Var1, y = value, fill = factor(Var2,labels=c("Yes"="Sim","No" = "Não")), label = Var1)) +
  theme_bw() + 
  geom_bar(stat = "identity",colour="black") +
  scale_x_discrete( labels = c("F" = "Feminino","M" = "Masculino")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values=c("grey50", "#723881")) +
  theme(text = element_text(size=15)) +
  labs(fill = "No Show") +
  ylab("Frequência Relativa")+
  xlab("Gênero")

pdf("GGen_NoShow.pdf", width = 20, height = 20) ;grid.arrange(g_Gender_1, g_Gender_2,ncol=2); dev.off()


##### Age #####


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

g_Age_1 <- ggplot(data, aes(x=Age,fill=No.show)) + theme_bw() +
  ylab("Frequência") + xlab("Idade") + 
  geom_bar(position = "stack",colour = "black") +
  theme(legend.position = "none") + 
  theme(text = element_text(size=20)) +
  scale_fill_manual(values=c("grey50", "#723881"))

 

g_Age_2 <- ggplot(filter(data,Age!=(-1)), aes(x=No.show, y=Age, fill=factor(No.show,labels=c("Yes"="Sim","No" = "Não")))) + 
  scale_x_discrete( labels = c("Yes" = "Sim","No" = "Não")) +
  theme_bw() + scale_fill_manual(values=c("grey50", "#723881")) +
  theme(text = element_text(size=20)) +
  labs(fill = "No Show") + xlab("No Show") +
  ylab("Idade") + geom_boxplot() 


pdf("Gidade_NoShow.pdf", width = 20, height = 20) ;grid.arrange(g_Age_1, g_Age_2,ncol=2); dev.off()


# observamos que temos idade de -1, que sao pacientes gravidas, elas frequentam mais nas consultas
  # pelo boxplot, tem uma concentracao maior no intervalo entre 15 ate 60, ou seja, tem mais jovem e adultos do que idosos, 
  # e tem dois outlier de 115 anos

AgeNoshowYes <- filter(data,No.show=="Yes")$Age
AgeNoshowNo <- filter(data,No.show=="No"& Age!=(-1))$Age

resumo_AgeNoshowYes <- c(summary(AgeNoshowYes),sd(AgeNoshowYes))
resumo_AgeNoshowNo <- c(summary(AgeNoshowNo),sd(AgeNoshowNo))

xtable(rbind(resumo_AgeNoshowYes,resumo_AgeNoshowNo))


faixas = c("0-5","6-18","19-60","61+")

bebe = which(Age<6)
crianca = which(Age>5 & Age<19)
adulto = which(Age>18 & Age<61)
idoso = which(Age>60)


data.faixa <- data.frame(x = c(rep("Primeira Infância(0-5)",length(bebe)),rep("Idade escolar(6-18)",length(crianca)),
                               rep("Adulto(19-60)",length(adulto)),rep("Idoso(61+)",length(idoso))))

data.faixa$x <- factor(data.faixa$x,levels = c("Primeira Infância(0-5)", "Idade escolar(6-18)","Adulto(19-60)", "Idoso(61+)"))


porcent.datafaixa <- data.frame(x = c("Primeira Infância(0-5)", "Idade escolar(6-18)","Adulto(19-60)", "Idoso(61+)"), 
              y = round(c(length(bebe),length(crianca), length(adulto),length(idoso))/length(data$Age),2))


g_faixa_1 <- ggplot(data.faixa) + 
                theme_bw() + 
                geom_bar(aes(x=x, y = ..prop..,  group = 1), fill = "gray", stat = "count" ,colour="black",position="dodge")+
                geom_text(data=porcent.datafaixa, aes(x = x, y = y+0.02,
                                   label = paste0(y*100,"%")), size=6) +
                scale_y_continuous(labels = scales::percent_format()) +
                theme(text = element_text(size=20)) +
                ylab("Frequência Relativa")+
                xlab("Faixa Etária")




# barplot(c(length(bebe), length(crianca), length(adulto),length(idoso))/length(Age), 
#         names = faixas)



faixa_etaria = rep(NA, length(Age))
faixa_etaria[bebe] = "Primeira Infância(0-5)"
faixa_etaria[crianca] = "Idade escolar(6-18)"
faixa_etaria[adulto] = "Adulto(19-60)"
faixa_etaria[idoso] = "Idoso(61+)"

data.Faixa.NoShow <- data.frame(t(t(table(No.show,faixa_etaria))/apply(table(No.show,faixa_etaria),2,sum)))

data.Faixa.NoShow$faixa_etaria  <- factor(data.Faixa.NoShow$faixa_etaria,
                                          levels = c("Primeira Infância(0-5)", "Idade escolar(6-18)", "Adulto(19-60)", "Idoso(61+)"))

data.Faixa.NoShow1 <- cbind(data.Faixa.NoShow, y = round(data.Faixa.NoShow$Freq,2), x = data.Faixa.NoShow$Freq + rep(c(0.02,-0.1),4 ))


g_faixa_2 <- ggplot(data.Faixa.NoShow1, aes(x=faixa_etaria,y=Freq,fill=factor(No.show,labels=c("No" = "Não","Yes"="Sim"))))+
              theme_bw() +
              geom_bar(stat="identity",colour="black")+
              geom_text(data=data.Faixa.NoShow1, aes(x = faixa_etaria, y = x,
                                        label = paste0(y*100,"%")), size=6) +
              scale_y_continuous(labels = scales::percent_format()) +
              scale_fill_manual(values=c("#723881","grey50")) +
              theme(text = element_text(size=20)) +
              labs(fill = "No Show") +
              ylab("Frequência Relativa por Faixa Etária")+
              xlab("Faixa Etária")



pdf("GFaixa_NoShow.pdf", width = 20, height = 20) ;grid.arrange(g_faixa_1, g_faixa_2,ncol=2); dev.off()


# barplot(table(No.show,faixa_etaria), beside = T)
# barplot(t(t(table(No.show,faixa_etaria))/apply(table(No.show,faixa_etaria),2,sum)), beside = T)


##### SMS #####

# add no-show e sms_received
tab_Sms <- table(data$SMS_received, data$No.show)
addmargins(tab_Sms)

# os que receberam a mensagem, 72% compareceu, os que nao receberam a mensagem, 83% compareceu, ou seja, nao ha muito efeito sobre a mensagem

xtable(prop.table(tab_Sms,1)*100,digits = 2)


##### 
#Diabetes

tab_diabetes <- table(data$Diabetes, data$No.show)
addmargins(tab_diabetes)

xtable(prop.table(tab_diabetes,1)*100,digits = 2)

#Alcoholism
tab_Alcoholism <- table(data$Alcoholism, data$No.show)
addmargins(tab_Alcoholism)

xtable(prop.table(tab_Alcoholism,1)*100,digits = 2)

#Hipertension

tab_Hipertension <- table(data$Hipertension, data$No.show)
addmargins(tab_Hipertension)

xtable(prop.table(tab_Hipertension,1)*100,digits = 2)

#Handcap

tab_Handcap <- table(data$Handcap, data$No.show)
addmargins(tab_Handcap)

xtable(prop.table(tab_Handcap,1)*100,digits = 2)

table(data$No.show)


#Scholarship

tab_Scholarship <- table(data$Scholarship, data$No.show)
addmargins(tab_Scholarship)

xtable(prop.table(tab_Scholarship,1)*100,digits = 2)

# add no-show e (Diabetes, Alcoholism, Hipertension, Handcap, Scholarship, SMS_received)
g_Diabetes <- ggplot(data, aes(x=Diabetes, fill=No.show)) + geom_bar(position="fill") +  theme_bw() +
  theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881")) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(text = element_text(size=20)) + 
  scale_x_discrete( labels = c("FALSE" = "Falso","TRUE" = "Verdadeiro")) + xlab("Diabetes") + ylab("Frequência Relativa")

g_Alcoholism <- ggplot(data, aes(x=Alcoholism, fill=No.show)) + geom_bar(position="fill") +  theme_bw() +
  theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881")) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(text = element_text(size=20)) +
  scale_x_discrete( labels = c("FALSE" = "Falso","TRUE" = "Verdadeiro")) + xlab("Alcoolismo") + ylab("Frequência Relativa")

g_Hipertension <- ggplot(data, aes(x=Hipertension, fill=factor(No.show,labels=c("Yes"="Sim","No" = "Não")))) +   theme_bw() +
  geom_bar(position="fill") + scale_fill_manual(values=c("grey50", "#723881")) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(text = element_text(size=20)) +
  scale_x_discrete( labels = c("FALSE" = "Falso","TRUE" = "Verdadeiro")) + xlab("Hipertensão ") + ylab("Frequência Relativa") +
  labs(fill = "No Show") 


g_Handcap <- ggplot(data, aes(x=Handcap, fill=No.show)) + geom_bar(position="fill") +  theme_bw() +
  theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881")) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(text = element_text(size=20)) +
  scale_x_discrete( labels = c("FALSE" = "Falso","TRUE" = "Verdadeiro")) + xlab("Deficiência") + ylab("Frequência Relativa") +
  labs(fill = "No Show") 

g_Scholarship <- ggplot(data, aes(x=Scholarship, fill=No.show)) + geom_bar(position="fill") +  theme_bw() +
  theme(legend.position = "none") + scale_fill_manual(values=c("grey50", "#723881")) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(text = element_text(size=20)) +
  scale_x_discrete( labels = c("FALSE" = "Falso","TRUE" = "Verdadeiro")) + xlab("Bolsa Família") + ylab("Frequência Relativa") +
  labs(fill = "No Show") 

g_SMS_received <- ggplot(data, aes(x=SMS_received, fill=factor(No.show,labels=c("Yes"="Sim","No" = "Não")))) +  theme_bw() +
  geom_bar(position="fill") + scale_fill_manual(values=c("grey50", "#723881")) +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(text = element_text(size=20)) +
  scale_x_discrete( labels = c("FALSE" = "Falso","TRUE" = "Verdadeiro")) + xlab("Recebeu SMS ") + ylab("Frequência Relativa") +
  labs(fill = "No Show")  

# Alcoholism eh praticamente igual, isso quer dizer que nao tem efeito no variavel resposta
# Diabetes, Hipertension, Scholarship, Handcap tem pequenas diferencas que nao sao relevantes
# SMS_received = quem que recebe mensagem tem mais proporcao que nao compareceu, ou seja, mensagem nao faz efeito

pdf("G31NoShow.pdf", width = 20, height = 20, paper = "a4r") # Open a new pdf file
grid.arrange(g_Diabetes, g_Alcoholism, g_Hipertension, ncol=3 ) 
dev.off()


pdf("G32NoShow.pdf", width = 20, height = 20, paper = "a4r") # Open a new pdf file
grid.arrange(g_Handcap, g_Scholarship, g_SMS_received, ncol=3 )
dev.off()




#add no-show e Neighbourhood

Freq_Neighbourhood <- filter(count(data$Neighbourhood), freq >=100) 

DadosNeighbourhood <- subset(data,Neighbourhood %in%  Freq_Neighbourhood$x, select = c(7,14)) %>% count() 

DadosNeighbourhood1 <- cbind(Yes=subset(DadosNeighbourhood, No.show =="Yes"), No=
                      subset(DadosNeighbourhood, No.show =="No",select =c(2,3)))

N <- DadosNeighbourhood1$Yes.freq + DadosNeighbourhood1$No.freq
taxaNo.show <- DadosNeighbourhood1$Yes.freq/N
neigh.N <- paste(DadosNeighbourhood1$Yes.Neighbourhood,"(",N,")")

DataNeig <- data.frame(neigh.N, taxaNo.show, taxar = round(taxaNo.show,3))

DataNeig <- DataNeig[order(taxaNo.show, decreasing = F),] 

DataNeig$neigh.N  <- factor(DataNeig$neigh.N,  levels = DataNeig$neigh.N )

medianoshow <- mean(DataNeig$taxaNo.show)

ggplot(DataNeig, aes(neigh.N, taxaNo.show, label = paste(taxar*100,"%"))) +   
  theme_bw() +
  geom_hline(aes(yintercept = medianoshow, color = "Taxa Média")) +
  geom_bar(stat = "identity", fill = "grey50", width = 0.5,colour = "black" ) +
  geom_text(nudge_y = 0.005,  size = 3) +
  scale_colour_manual(" ",values = c("black")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(text = element_text(size=13)) +
  theme(legend.position = c(0.75, 0.2), legend.direction = "horizontal") +
  ylab("Taxa de No-Show") + xlab("Endereço da consulta ( n = número de agendamentos)") + 
  coord_flip() 



  # No Show e dias da semana

datatransf <- read.table("dados_no_show.csv", header=T, sep=",")

datatempo <- subset(datatransf,select = c("No.show","Wait","Hour","Day_Week"))


datanew <- data.frame(table(datatempo[,c(1,4)]))

cbind(Yes = subset(datanew,No.show=="Yes"),No =subset( datanew,No.show=="No", Freq))

DataNeig$neigh.N  <- factor(datanew$Day_Week,  levels = c("Monday","Tuesday","Wednesday",
                                                         "Thursday", "Friday", "Saturday" ) )









g_neigh_amount <- ggplot(data,aes(x=Neighbourhood,fill=factor(No.show,labels=c("Yes"="Sim","No" = "Não"))))+
  geom_bar(position="stack")+
  theme_bw() +
  scale_fill_manual(values=c("grey50","#723881"))+
  theme(axis.text.x = element_text(angle=45, hjust=1,size=5))+
  ylab("Frequência") +
  xlab("Endereço da Consulta") +
  theme(text = element_text(size=20)) +
  labs(fill = "No Show") +
  ggtitle("No Show versus endereço da consulta")

g_neigh_prop <- ggplot(data,aes(x=Neighbourhood,fill=factor(No.show,labels=c("Yes"="Sim","No" = "Não"))))+
  theme_bw() +
  geom_bar(position="fill")+
  scale_fill_manual(values=c("grey50","#723881"))+
  theme(axis.text.x = element_text(angle=45, hjust=1,size=5))+
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(text = element_text(size=20)) +
  ylab("Proporção") +
  xlab("Endereço da Consulta") +
  labs(fill = "No Show") +
  ggtitle("Proporção de No Show versus endereço da consulta")

# se temos mais informacoes como a distancia entre o lugar da consulta e o endereco do paciente, vai sair mais informacoes,
# tem alguns bairros tem poucas informacoes, portanto nao da para entrar em consideracao
# tem que ver se esse endereco eh do hospital? 
pdf("GEndConsultaNoShow.pdf", width = 20, height = 20, paper = "a4r") # Open a new pdf file
grid.arrange(g_neigh_amount, g_neigh_prop, nrow=2)
dev.off()





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

#Distribuicao do tempo
boxplot(tempo~No.show)
hist(tempo,freq = F, col="green", nc=30)
