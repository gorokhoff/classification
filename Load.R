#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
#if(!exists("data")){
  data<-read.csv(file = 'data/train.csv', header = TRUE, sep = ',', na.strings=c("", "NA", "NULL"))
#}

  data$x29[is.na(data$x29)]<-44.65
  data$x38[is.na(data$x38)]<-0.78
  data$x40[is.na(data$x40)]<-35.055
  data$x61[is.na(data$x61)]<-164
  data$x8[is.na(data$x8)]<-0.2272
  
  dt<-data[complete.cases(data),]
#   dt$x26a<-round(dt$x26*12)
#   dt$x26a<-as.factor(dt$x26a)
#   dt$x26<-dt$x26a
  
#   dt$x29a<-round(dt$x29*12)
#   dt$x29a<-as.factor(dt$x29a)
#   dt$x29<-dt$x29a
#   

  dt$y <- as.factor(dt$y)
  #Anomaly replace
  dt$x29[dt$x29 > 4000]<-44.65
  dt$x38[dt$x38 > 32]<-0.78
  dt$x40[dt$x40 > 500]<-35.055
  dt$x61[dt$x61 > 2600]<-164
  dt$x8[dt$x8 < -3]<-0.2272
  #ss<-subset(dt, dt$x38 < 43)
  
# dt$x35a<-round(dt$x35*12)
# dt$x35a<-as.factor(dt$x35a)
# dt$x35<-dt$x35a
  drops <- c("x6","x1","x2","x3", "x4","x7")
  dt<-dt[,!(names(dt) %in% drops)]
  sub.0<-subset(dt,y=="0")
  sub.1<-subset(dt,y=="1")
  sub.2<-subset(dt,y=="2")
  sub.3<-subset(dt,y=="3")
  sub.4<-subset(dt,y=="4")
  sub.5<-subset(dt,y=="5")
  sub.6<-subset(dt,y=="6")

  idt<-dropBind(dt,model.matrix(~x5,dt))
  idt<-dropBind(idt,model.matrix(~x9,dt))
  idt<-dropBind(idt,model.matrix(~x0,dt))
  idt<-dropBind(idt,model.matrix(~x11,dt))
  idt<-dropBind(idt,model.matrix(~x12,dt))
  idt<-dropBind(idt,model.matrix(~x14,dt))
  idt<-dropBind(idt,model.matrix(~x15,dt))
  idt<-dropBind(idt,model.matrix(~x10,dt))
  idt<-dropBind(idt,model.matrix(~x16,dt))
  idt<-dropBind(idt,model.matrix(~x17,dt))
  idt<-dropBind(idt,model.matrix(~x18,dt))
  idt<-dropBind(idt,model.matrix(~x19,dt))
  idt<-dropBind(idt,model.matrix(~x20,dt))
  idt<-dropBind(idt,model.matrix(~x21,dt))
  idt<-dropBind(idt,model.matrix(~x22,dt))
  drops <- c("x5","x0","x9","x10","x11","x12","x14","x15","x16","x17","x18","x19","x20","x21","x22")
  idt<-idt[,!(names(idt) %in% drops)]
  
  #library(FactoMineR)
  #dt.pca<-prcomp(dt)
  #install.packages("randomForest")
  library(randomForest)


#  numIndex<-sapply(sub.0, is.numeric)
#  factorIndex<-sapply(sub.0, is.factor)

set.seed(555)
  #drops <- c("x10","x0","x18","x17","x20", "x21")
  #dtrf<-dt[,!(names(dt) %in% drops)]
split <- runif(dim(idt)[1]) > 0.2
train <- idt[split,]
test <- idt[!split,]

#model_glm1 <- glm(y ~ ., data = test, family = binomial)
#gc()
  
  #model.rf<-randomForest(y ~., train, importance = TRUE)
  model.pr<-randomForest(y ~., train, importance = TRUE, proximity=TRUE)
#predictions <- predict(model, test, type = "response")
#print(sqrt(sum((as.vector(predictions - test$y))^2))/length(predictions))

#summary(sub0)

