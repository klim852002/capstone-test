# Load packages
source("C:\\Users\\Jerome\\Desktop\\JeromeR.R")


library(keras)
library(EBImage)
library(wvtool)


test = readImage(trainfiles[1])
test2 <- resize(test, 100, 100)
hist(test2)

setwd('C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\')
img = readImage("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps1_T\\slide001_core045_classimg_nonconvex.png")
img2 <- resize(img, 500, 500)
wtf(img2@.Data)
hist(img2)


test2 <- array_reshape(test, c(500,500,3))
test2 <- as.matrix(resize(test, 1000, 1000))

edge.detect(test2)

# Read Images
setwd('C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Train_Imgs')
trainfiles <- list.files(path = "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Train_Imgs", 
                    pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, 
                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

train <- list()
for (i in 1:10) {train[[i]] <- readImage(trainfiles[i])}

testfiles <- list.files(path = "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Test_Imgs", 
                         pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, 
                         ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

test <- list()
for (i in 1:length(testfiles)) {test[[i]] <- readImage(testfiles[i])}






# Explore
print(train[[10]])
summary(train[[10]])
display(train[[10]])
plot(train[[2]])
hist(train[[1]])

# Resize & combine
str(train)
for (i in 1:10) {train[[i]] <- resize(train[[i]], 100, 100)}

for (i in 1:3) {test[[i]] <- resize(test[[i]], 100, 100)}

train <- combine(train)
x <- tile(train, 5)
display(x, title='Pictures')

test <- ---(test)
y <- ---(test, 3)
---(y, title = 'Pics')

# Reorder dimension
train <- ---(train, c(4, 1, 2, 3))
test <- ---(test, c(4, 1, 2, 3))
str(train)

# Response
trainy <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
testy <- c(0, 1, 2)

# One hot encoding
trainLabels <- ---(trainy)
testLabels <- ---(testy)

# Model
model <- keras_model_sequential()

model %>%
  layer_conv_2d(filters = 32, 
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(100, 100, 3)) %>%
  (filters = 32, kernel_size = c(3,3),
      activation = 'relu') %>%
  ---(pool_size = c(2,2)) %>%
  ---(rate = 0.25) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  ---() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate=0.25) %>%
  layer_dense(units = 3, activation = '---') %>%
  
  compile(loss = '---',
          optimizer = optimizer_sgd(lr = 0.01,
                                    decay = 1e-6,
                                    momentum = 0.9,
                                    nesterov = T),
          metrics = c('---'))
summary(model)

# Fit model
history <- model %>%
  fit(train,
      trainLabels,
      --- = 60,
      --- = 32,
      validation_split = 0.2,
      validation_data = list(test, testLabels))
plot(history)

# Evaluation & Prediction - train data
model %>% ---(train, trainLabels)
pred <- model %>% ---(train)
table(Predicted = pred, Actual = trainy)

prob <- model %>% ---(train)
cbind(prob, Predicted_class = pred, Actual = trainy)

# Evaluation & Prediction - test data
model %>% ---(test, testLabels)
pred <- model %>% ---(test)
table(Predicted = pred, Actual = testy)

prob <- model %>% ---(test)
---(prob, Predicted_class = pred, Actual = testy)





### Basic Image Recognition
# Load Packages
library(EBImage)
library(keras)

# Read images
setwd('---')
pics <- c('p1.jpg', 'p2.jpg', 'p3.jpg', 'p4.jpg', 'p5.jpg', 'p6.jpg',
          'c1.jpg', 'c2.jpg', 'c3.jpg', 'c4.jpg', 'c5.jpg', 'c6.jpg')
mypic <- list()
for (i in 1:12) {mypic[[i]] <- readImage(pics[i])}

# Explore
print(mypic[[1]])
display(mypic[[8]])
summary(mypic[[1]])
hist(mypic[[2]])
str(mypic)

# Resize
for (i in 1:12) {mypic[[i]] <- resize(mypic[[i]], ---, ---)}

# Reshape
for (i in 1:12) {mypic[[i]] <- array_reshape(mypic[[i]], c(---, ---,---))}

# Row Bind
trainx <- NULL
for (i in 7:11) {trainx <- rbind(trainx, mypic[[i]])}
str(trainx)
testx <- ---(mypic[[6]], mypic[[12]])
trainy <- c(0,0,0,0,0,1,1,1,1,1 )
testy <- c(---, ---)

# One Hot Encoding
trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)

# Model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = ---, input_shape = c(2352)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 2, activation = ---)
summary(model)

# Compile
model %>%
  compile(loss = ---,
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy'))

# Fit Model
history <- model %>%
  fit(trainx,
      ---,
      epochs = 30,
      batch_size = 32,
      validation_split = 0.2)

# Evaluation & Prediction - train data
model %>% evaluate(---, ---)
pred <- model %>% predict_classes(trainx)
table(Predicted = pred, Actual = trainy)
prob <- model %>% predict_proba(trainx)
cbind(prob, Prected = pred, Actual= trainy)



data(camphora)
data(cryptomeria)
cryptomeria <- rgb2gray(cryptomeria)
dim(cryptomeria)

img.c1 <- crop(camphora,200,200)
img.c2 <- crop(cryptomeria,300,300)
par(mfrow=c(1,1))
image(rot90c(edge.detect(img.c1,thresh1=1, thresh2=15, noise="gaussian", noise.s=3,
 method="Canny")),col=gray(c(0:255)/255), main="Canny", useRaster=TRUE, axes=FALSE, asp=1)
