library(MASS)

dados = read.table("treinamento.csv", header=T, sep=",")
head(dados)
summary(dados)
attach(dados)

lda.fit <- lda(No.show ~ Gender+Age+Neighbourhood+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap
               +SMS_received+Wait+Hour+Day_Week+Day_Week_Appointment,
               data = dados)
lda.fit
lda.pred <- predict(lda.fit, dados)
str(lda.pred)

lda.pred$class

table(lda.pred$class, dados$No.show)

mean(lda.pred$class == dados$No.show, na.rm=TRUE)


teste = read.table("teste.csv", header=T, sep=",")
head(teste)

lda.pred <- predict(lda.fit, teste)

table(lda.pred$class, teste$No.show)

mean(lda.pred$class == teste$No.show, na.rm=TRUE)


# QDA
qda.fit <- qda(No.show ~ Gender+Age+Neighbourhood+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap
              +SMS_received+Wait+Hour+Day_Week+Day_Week_Appointment,
                data = dados)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
mean(qda.class == Direction.2005)

resultados[nrow(resultados)+1,] <- c("QDA (separacao treino/teste)",
                                     mean(qda.class == Direction.2005))

# KNN
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
mean(knn.pred == Direction.2005)

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
mean(knn.pred == Direction.2005)


resultados[nrow(resultados)+1,] <- c("KNN k=3",
                                     mean(knn.pred == Direction.2005))

# tarefa: verificar qual valor de k maximiza a taxa de acerto
k = seq(1:10)

resultados