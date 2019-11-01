rm(list = ls())
library(dplyr)
library(xtable)
library(reshape2) 
require(plyr)

dados.trat <- read.table("treinamento.csv", header=T, sep=",")
dados.trat <- dados.trat[-which(is.na(dados.trat$Wait)),]
dados.test <- read.table("teste.csv", header=T, sep=",")
dados.test <-  dados.test[-which(is.na(dados.test$Wait)),]


data <- rbind(dados.trat,dados.test) %>% as.data.frame()

head(data)
attach(data)


tree.no_show <- rpart(formula = No.show ~ Gender + Age  + Scholarship + Hipertension + Diabetes + Alcoholism + Handcap
                      + SMS_received + Wait, data = data,method = "class", maxdepth =4,
                      minsplit = 2,
                      minbucket = 1,
                      cp=-1)


rpart.plot(tree.no_show)
tree.no_show 
tree.no_show$cptable

table(predict(tree.no_show,type = "class"))


tree.no_show.trat <-  rpart(formula = No.show ~ Gender + Age  + Scholarship + Hipertension + Diabetes + Alcoholism + Handcap
                            + SMS_received + Wait, data = dados.trat,method = "class",maxdepth =4,
                            minsplit = 2,
                            minbucket = 1,
                            cp=-1)


tree.pred <- predict(tree.no_show.trat, dados.test, type = "class")


table(tree.pred, dados.test$No.show)

sum(diag(table(tree.pred, dados.test$No.show)))/ nrow(dados.test) # acuracia

mean(tree.pred == dados.test$No.show)

rpart.plot(tree.no_show.trat , cex = 0.9)

tree.no_show.trat
tree.no_show.trat$cptable


