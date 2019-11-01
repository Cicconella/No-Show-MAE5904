data <- read.table("noshowappointments/KaggleV2-May-2016.csv",
                   header=T, sep=",")

#frequencia de consultas por pacientes

Freq_consul <- count(data$PatientId)
Freq_consul <- Freq_consul[order(Freq_consul$freq),]

#Pacientes com uma consulta

data1 <- filter(data, PatientId %in% Freq_consul[Freq_consul$freq == 1,1])

#Pacientes com mais do que uma consulta

data2 <- filter(data, PatientId %in% Freq_consul[Freq_consul$freq != 1,1])

#Como as datas dos agendamentos não é ordenado, temos que ordenar dentro do número de 
#consultas de cada paciten

data2[1:25,1:6]

data3 <- data2[order(data2$PatientId,as.Date(data2$ScheduledDay, format ="%Y-%m-%dT%H:%M:%SZ")),]
data3[1:25,1:6]
