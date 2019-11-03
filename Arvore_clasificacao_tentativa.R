rm(list=ls())
library(zoo)
library(rmarkdown)
library(knitr)
library(pROC)
library(rpart)
library(rpart.plot)
options(scipen = 999)

th <- read.table("noshowappointments/KaggleV2-May-2016.csv",
                   header=T, sep=",")

# recode the data
df <- data.frame(
  
  # PATIENT PROPERTIES
  patid = th$PatientId,
  male = ifelse(th$Gender=="M",1,0),
  age = th$Age,
  burs = th$Scholarship,
  hiper = th$Hipertension,
  diabet = th$Diabetes,
  alcohol = th$Alcoholism,
  handcap = th$Handcap,
  rayon = th$Neighbourhood,
  
  # APPOINTMENT PROPERTIES
  appid = th$AppointmentID,
  
  daysc = as.Date(th$ScheduledDay),
  wdaysc = strptime(th$ScheduledDay,"%Y-%m-%dT%H:%M:%SZ","GMT")$wday,
  hoursc = strptime(th$ScheduledDay,"%Y-%m-%dT%H:%M:%SZ","GMT")$hour,
  
  dayap = as.Date(th$AppointmentDay),
  wdayap = strptime(th$AppointmentDay,"%Y-%m-%dT%H:%M:%SZ","GMT")$wday,
  
  sms = th$SMS_received,
  noshow = ifelse(th$No.show=="Yes",1,0)
)

dim(df) 


#df <- df[ (df$age >= 0) & (df$age <= 97) & (df$dayap >= df$daysc),]
df <- df[ (df$age >= 0) & (df$dayap >= df$daysc),]

df$daybw <- as.numeric(df$dayap - df$daysc)
df$adult <- ifelse(df$age>=18,1,0)


df$solbor <- ifelse(df$rayon=="SOLON BORGES",1,0)
df$sandum <- ifelse(df$rayon=="SANTOS DUMONT",1,0)
df$sancla <- ifelse(df$rayon=="SANTA CLARA",1,0)
df$sancec <- ifelse(df$rayon=="SANTA CECÍLIA",1,0)
df$itarar <- ifelse(df$rayon=="ITARARÉ",1,0)
df$lourde <- ifelse(df$rayon=="DE LOURDES",1,0)
df$cabral <- ifelse(df$rayon=="DO CABRAL",1,0)
df$quadro <- ifelse(df$rayon=="DO QUADRO",1,0)
df$horto  <- ifelse(df$rayon=="HORTO",1,0)
df$penha  <- ifelse(df$rayon=="JARDIM DA PENHA",1,0)
df$jesus  <- ifelse(df$rayon=="JESUS DE NAZARETH",1,0)
df$cypres <- ifelse(df$rayon=="MÁRIO CYPRESTE",1,0)
df$sanmar <- ifelse(df$rayon=="SANTA MARTHA",1,0)

df <- df[order(df$patid, df$dayap),]
df$prev <- rep(0,nrow(df))
df$noprev <- rep(0,nrow(df))
for(i in 2:nrow(df)){
  if(df$patid[i] == df$patid[i-1]){
    df$prev[i] <- df$prev[i-1]+1
    df$noprev[i] <- df$noprev[i-1] + df$noshow[i-1]
  }
}
df$noprevpct <- ifelse(is.nan(df$noprev/df$prev),0,df$noprev/df$prev)
df$mode_previous <- ifelse(df$noprevpct>=0.5, 1, 0)

df <- df[order(df$dayap),]
df$prior <- c(0,cumsum(df$noshow))[-nrow(df)]/c(1:nrow(df))
plot(df$prior, type="n")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#e5e5e5")
grid(col="white", lwd=2)


df$waittime <- ifelse(df$daybw==0,0,ifelse(df$daybw<=7,1,2))

df$morning <- ifelse(df$hoursc>=6 & df$hoursc<=10, 1, 0)

df$age18 <- ifelse(df$age>=18,1,0)
df$age80 <- ifelse(df$age>=80,1,0)
df$age_decade <- floor(df$age/10)

#df <- df[order(df$dayap),]
trainsize <- round(nrow(df)*0.2)
dftrain <- df[1:trainsize,]
dftest <- df[(trainsize+1):(trainsize*2),]

table(dftrain$noshow)

dim(df)


zf <- rbind(df[df$noshow==0,][sample(nrow(df[df$noshow==0,]),nrow(df[df$noshow==1,])),], df[df$noshow==1,])


dim(zf)

table(zf$noshow)


dtree <- rpart(formula=noshow ~ age_decade +
                 burs + diabet + handcap + alcohol + hiper 
               ,
               data = zf,
               method = "class",
               maxdepth =3,
               minsplit = 2,
               minbucket = 1,
               cp=-1
)
rpart.plot(dtree)




predicted_noshow <- ifelse(predict(dtree, newdata = df)[,2]>=0.5,1,0)
actual_noshow <- df$noshow

mytab <- table(abs(predicted_noshow - actual_noshow))

accuracy <- mytab[1]/(mytab[1] + mytab[2])
accuracy
roc(actual_noshow, predicted_noshow)

table(predicted_noshow, actual_noshow)[2,2] / (table(predicted_noshow, actual_noshow)[1,2] + table(predicted_noshow, actual_noshow)[2,2])



logit <- glm(formula =
               noshow ~ age + age18 + age80 + male + wdaysc + wdayap +
               burs + diabet + handcap + alcohol +
               solbor + sandum + itarar + lourde + cabral + quadro + penha + jesus + sanmar +
               sms + morning + waittime + mode_previous,
             family = binomial(link="logit"), data = zf, )

summary(logit)
# 
predicted_noshow <- ifelse(predict(logit, df,type = "response")>=0.5,1,0)
actual_noshow <- df$noshow
mytab <- table(abs(predicted_noshow - actual_noshow))

accuracy <- mytab[1]/(mytab[1] + mytab[2])
accuracy
roc(actual_noshow, predicted_noshow)