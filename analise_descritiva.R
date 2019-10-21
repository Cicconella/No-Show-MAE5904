dados = read.table("noshowappointments/KaggleV2-May-2016.csv", stringsAsFactors = F,
                   header=T, sep=",")
head(dados)
attach(dados)


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


##### Genero #####
colnames(dados)

table(Gender)

barplot(table(Gender))
barplot(table(Gender,No.show), beside = T)

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


