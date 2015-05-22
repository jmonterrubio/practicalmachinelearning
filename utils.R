library(caret)
library(rattle)

readsubdataset <- function(datasettype = "training", directory = "./") {
      #
      # Reads and combine the data of a data set type in a data.frame
      #
      # Args:
      #   datasettype: training or testing
      #   directory  : base folder for read
      #
      # Returns:
      #   the combined data
      #
      # Read dataset
      dataset <- read.table(paste(directory,"pml-",datasettype,".csv",sep = ""),
                            header = T,sep = ",",
                            na.strings=c("NA", "", " "))
      data.frame(dataset)
}

cleandataset <- function(df) { 
  clean <- df[, colSums(is.na(df)) ==  0]
  drops <- c("cvtd_timestamp",
             "raw_timestamp_part_1",
             "raw_timestamp_part_2",
             "X",
             "new_window")
  clean[,!(names(clean) %in% drops)]
}

pml_write_files <- function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}
set.seed(666)
training <- readsubdataset(directory="practicalmachinelearning/")
clean <- cleandataset(training)

inTrain <- createDataPartition(y=clean$classe, p=0.7, list=FALSE)
training1 <- clean[inTrain,]
testing1 <- clean[-inTrain,]
qplot(user_name,colour=classe,data=training1)
mod1 <- train(classe ~ ., method="rpart", data=training1)
print(mod1$finalModel)
predictions1 <- predict(mod1,newdata=testing1)
confusionMatrix(predictions1, testing1$classe)

####

train_control2 <- trainControl(method="boot", number=100)
mod2 <- train(classe ~ ., data=clean, trControl = train_control2, method="rpart")
print(mod2$finalModel)
predictions2 <- predict(mod2,newdata=clean)
confusionMatrix(predictions2, clean$classe)

####

train_control3 <- trainControl(method="cv", number=10)
mod3 <- train(classe ~ ., data=clean, trControl = train_control3, method="rpart")
predictions3 <- predict(mod3,newdata=clean)
confusionMatrix(predictions3, clean$classe)

####

train_control4 <- trainControl(method="repeatedcv", number=10, repeats=3)
mod4 <- train(classe ~ ., data=clean, trControl = train_control4, method="rpart")
predictions4 <- predict(mod4,newdata=clean)
confusionMatrix(predictions4, clean$classe)

####

train_control5 <- trainControl(method="cv", number=10)
mod5 <- train(classe ~ ., data=clean, trControl = train_control5, method="lm")
predictions5 <- predict(mod5,newdata=clean)
confusionMatrix(predictions5, clean$classe)