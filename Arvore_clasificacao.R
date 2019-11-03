rm(list = ls())
library(rpart)          # decision tree methodology
library(rpart.plot)     # decision tree visualization
library(dplyr)
library(xtable)
require(plyr)

dados.trat <- read.table("treinamento.csv", header=T, sep=",") %>% mutate(No.show = ifelse(No.show=="Yes","No-Show","Show")) 
dim(dados.trat)
dados.trat <- dados.trat[-which(is.na(dados.trat$Wait)),]
dim(dados.trat)
dados.test <- read.table("teste.csv", header=T, sep=",") %>% mutate(No.show = ifelse(No.show=="Yes","No-Show","Show")) 
dim(dados.test)
which(is.na(dados.test$Wait))

data <- rbind.data.frame(dados.trat,dados.test)


table(data$No.show)
dim(data)

head(data)
attach(data)

table(data$No.show)

tree.no_show <- rpart(formula = No.show ~ Gender + Age  + Scholarship + Hipertension + Diabetes + Alcoholism + Handcap
                      + SMS_received + Wait, data = data,method = "class",maxdepth =3,
                      minsplit = 2,
                      minbucket = 1,
                      cp=-1)

rpart.plot(tree.no_show, cex = 0.9)
tree.no_show 
tree.no_show$cptable

table(predict(tree.no_show,type = "class"))


tree.no_show.trat <-  rpart(formula = No.show ~ Gender + Age  + Scholarship + Hipertension + Diabetes + Alcoholism + Handcap
                            + SMS_received + Wait, data = dados.trat,method = "class",maxdepth =3,
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


