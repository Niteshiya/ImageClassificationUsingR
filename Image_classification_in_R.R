#load Libraries
library(keras)
library(EBImage)
#setting working diretory
setwd("C:\\Users\\Nitesh\\Desktop\\r datasets and practice\\image_class")
#Taking out data and loading it
pics <- c("c1.jpg","c2.jpg","c3.jpg","c4.jpg","c5.jpg","c6.jpg",
          "m1.jpg","m2.jpg","m3.jpg","m4.jpg","m5.jpg","m6.jpg")
my_pic <- c()
for(i in 1:12){
  my_pic[[i]] <- readImage(pics[i]) 
}
#Exploration of my_pic
print(my_pic[[1]])
display(my_pic[[8]])
summary(my_pic[[1]])
hist(my_pic[[2]])
str(my_pic)
#All pictures have diffrent dimentions length*wide*color
#Lets resize all the pictures that we have
for(i in 1:12){
  my_pic[[i]] <- resize(my_pic[[i]],120,120)
}
display(my_pic[[2]])
str(my_pic)
#Now all the pictures have the same dimention that is 120*120*3
#For traning purpose we need to shape all images into 1D array of dim(l*b*color,1)
for(i in 1:12){
  my-pic[[i]] <- array_reshape(my_pic[[i]],c(120,120,3))
}
str(my_pic)
#Now we will combine data for traning and testing
trainx <- NULL
for(i in 1:5){
  trainx <- rbind(trainx,my_pic[[i]])
}
str(trainx)
for(i in 7:11){
  trainx <- rbind(trainx,my_pic[[i]])
}
str(trainx)
testx <- rbind(my_pic[[6]],my_pic[[12]])
#Cat = 1 mouse =0
trainy <- c(1,1,1,1,1,0,0,0,0,0)
testy <- c(1,0)
#One-Hot-Encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)
head(trainLabels)
#Creating a model
model <- keras_model_sequential()
model %>% 
  layer_dense(units=100,activation = "sigmoid",input_shape =43200) %>%
  layer_dense(units=50,activation = "relu",input_shape = 100) %>%
  layer_dense(units=2,activation ="softmax")
summary(model)
#Compile the model
model %>%
  compile(loss="binary_crossentropy",
          optimizer = optimizer_rmsprop(),
          metrics="accuracy")
#Fit model
history <- model %>%
  fit(trainx,trainLabels,epochs=150,batch_size=32,validation_split=0.2)
#For better view plot history
plot(history)
#Lets go for evaluation and prediction
model %>%
  evaluate(trainx,trainLabels)
pred <- model %>%
  predict_classes(trainx)
table(predicted=pred,actual=trainy)
prob <- model %>% predict_proba(trainx)
cbind(prob,predicted=pred,actual =trainy)
#Evaluation and prediction on test data
model %>% evaluate(testx,testLabels)
pred <- model %>% predict_classes(testx)
table(predicted=pred,actual=testy)
