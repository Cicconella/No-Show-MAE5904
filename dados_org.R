library(plyr)

data <- read.table("noshowappointments/KaggleV2-May-2016.csv",
                   header=T, sep=",")

#Como as datas dos agendamentos não é ordenado, temos que ordenar dentro do número de 
#consultas de cada pacientes

data[1:25,1:6]

data2 <- data[order(data$PatientId,as.Date(data$ScheduledDay, format ="%Y-%m-%dT%H:%M:%SZ")),]
data2[1:25,1:6]

dups = which(duplicated(data2$PatientId))
dups

data3 = data2[-dups,]
dim(data3)

write.table(data3,"dados_filtrados.csv",quote = F, col.names = T, row.names = F, sep=",")

