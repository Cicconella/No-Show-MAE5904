dados = read.table("noshowappointments/KaggleV2-May-2016.csv", stringsAsFactors = F,
                   header=T, sep=",")
head(dados)
attach(dados)

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

which(tempo<0)
