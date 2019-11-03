
library(glmnet)
library(e1071)
dados_1 <- dados[-c(dados$PatientId,dados$AppointmentID)]

fit1m=glmnet(as.matrix(dados_1),dados$No.show,family="binomial")

library(caret)

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model <- train( No.show ~ .,
               data = dados_1,
               trControl = train_control,
               method = "glm",
               family=binomial())

# print cv scores
summary(model)


# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(No.show ~., data=dados_1, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
