dados = read.table("dados_filtrados.csv", header=T, sep=",", stringsAsFactors = F)
head(dados)
summary(dados)
attach(dados)

#### Colocar fatores ####
dados$Scholarship[dados$Scholarship=="0"] = "No"
dados$Scholarship[dados$Scholarship=="1"] = "Yes"

dados$Hipertension[dados$Hipertension=="0"] = "No"
dados$Hipertension[dados$Hipertension=="1"] = "Yes"

dados$Diabetes[dados$Diabetes=="0"] = "No"
dados$Diabetes[dados$Diabetes=="1"] = "Yes"

dados$Alcoholism[dados$Alcoholism=="0"] = "No"
dados$Alcoholism[dados$Alcoholism=="1"] = "Yes"

dados$Handcap[dados$Handcap=="0"] = "No"
dados$Handcap[dados$Handcap=="1"] = "Mild"
dados$Handcap[dados$Handcap=="2"] = "Moderate"
dados$Handcap[dados$Handcap=="3"] = "Severe"
dados$Handcap[dados$Handcap=="4"] = "Very_Severe"

dados$SMS_received[dados$SMS_received=="0"] = "No"
dados$SMS_received[dados$SMS_received=="1"] = "Yes"

#### Transformar age para faixa etária ####

faixas = c("0-5","6-18","19-60","61+")

#Observacoes negativas (gravidas) foram colocadas como bebe

bebe = which(Age<6)
crianca = which(Age>5 & Age<19)
adulto = which(Age>18 & Age<61)
idoso = which(Age>60)

dados$Age[bebe] = "Infant"
dados$Age[crianca] = "Schooler"
dados$Age[adulto] = "Adult"
dados$Age[idoso] = "Elderly"

head(dados)

#### Correcao tempo entre agendamento e consulta ####

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

#### Horario em que a Consulta  foi marcada####

horario = unlist(strsplit(ScheduledDay, split = "T")) 
horario = horario[seq(2,length(horario),by=2)]
horario = gsub("Z","",horario)
horario[12000:12020]

tt <- strptime(paste("2001-01-01", horario), format="%Y-%m-%d %H:%M:%S")

horario = format(round(tt, units="hours"), format="%H:%M:%S")

horario = unlist(strsplit(horario, split = ":")) 
horario = horario[seq(1,length(horario),by=3)]
horario = as.numeric(horario)
horario

madrugada = which(horario<9)
manha = which(horario>8 & horario<13)
tarde = which(horario>12 & horario<19)
noite = which(horario>18)

Hour = rep(NA, nrow(dados))
Hour[madrugada] = "Dawn"
Hour[manha] = "Morning"
Hour[tarde] = "Afternoon"
Hour[noite] = "Night"

head(dados)
dados = cbind(dados,Hour)
head(dados)

#### Dia da Semana ####

dia = gsub("T"," ",AppointmentDay)
dia = gsub("Z","",dia)

dia = strptime(dia, "%Y-%m-%d %H:%M:%S")
dia

Day_Week = weekdays(dia)
table(Day_Week)

dados = cbind(dados, Day_Week)
head(dados)

dados[19745,]

dia = gsub("T"," ",ScheduledDay)
dia = gsub("Z","",dia)

dia = strptime(dia, "%Y-%m-%d %H:%M:%S")
dia

Day_Week_Appointment = weekdays(dia)
table(Day_Week)

dados = cbind(dados, Day_Week_Appointment)
head(dados)

dados[19745,]

#### Excluir variáveis ScheduledDay e AppointmentDay #####

dados = dados[,-c(4,5)]
head(dados)

#Amostras com erro 

summary(dados)

# 5 amostras de wait são negativas, entao colocaremos como NA 
which(dados$Wait<0)

dados$Wait[which(dados$Wait<0)]=NA

write.table(dados,"dados_no_show.csv", col.names = T, row.names = F, quote = F, sep=",")

#### Arquivo de treinamento e teste####

dim(dados)

n = nrow(dados)

treinamento = sample(seq(1,n))[1:(n*0.8)]
length(treinamento)

p = table(dados$No.show[treinamento])
p/sum(p)

teste = seq(1,n)[-treinamento]
teste
length(teste)

p = table(dados$No.show[teste])
p/sum(p)

write.table(dados[treinamento,], "treinamento.csv", col.names = T, row.names = F, quote = F, 
            sep=",")

write.table(dados[teste,], "teste.csv", col.names = T, row.names = F, quote = F, sep=",")


##### Validação #####
