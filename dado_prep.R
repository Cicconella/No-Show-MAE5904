dados = read.table("noshowappointments/KaggleV2-May-2016.csv", header=T, sep=",", stringsAsFactors = F)
head(dados)
attach(dados)

#### Transformar age para faixa etária ####

faixas = c("0-5","6-18","19-60","61+")

#Observacoes negativas (gravidas) foram colocadas como bebe

bebe = which(Age<6)
crianca = which(Age>5 & Age<19)
adulto = which(Age>18 & Age<61)
idoso = which(Age>60)

dados$Age[bebe] = 1
dados$Age[crianca] = 2
dados$Age[adulto] = 3
dados$Age[idoso] = 4

head(dados)

#### Correcao tempo entre agendamento e consulta ####

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
Wait = as.integer(tempo)

dados = cbind(dados,Wait)
head(dados)

#### Horario da Consulta ####

horario = unlist(strsplit(ScheduledDay, split = "T")) 
horario = horario[seq(2,length(horario),by=2)]
horario = gsub("Z","",horario)
horario

horario = unlist(strsplit(horario, split = ":")) 
horario = horario[seq(1,length(horario),by=3)]
horario = as.numeric(horario)
horario

madrugada = which(horario<9)
manha = which(horario>8 & )
tarde
noite


#### Dia da Semana ####


#### 

#Amostras com erro 
data$ScheduledDay[which(tempo<0)] = NA
data$AppointmentDay[which(tempo<0)] = NA
