# Load packages
library(keras)
library(EBImage)
library(wvtool)
wtf <- function(x) {
  tmpfile = strsplit(tempfile(), split = "[\\]") %>% unlist %>% head(7) %>% paste(collapse = '/')
  tempfile2 = deparse(substitute(x))
  fn = paste(tmpfile, tempfile2, sep = '/')
  x2 = as.data.frame(x)
  out = try(write.table(x2, file = paste0(fn,".csv"), sep = ",", row.names = F, col.names = T))
  if (class(out) == "try-error") {
    DefaultTotemp <- tempfile()
    write.table(x2, file = paste0(DefaultTotemp,".csv"), sep = ",", row.names = F, col.names = T)
    shell.exec(paste0(DefaultTotemp, ".csv"))
  } else {
    shell.exec(paste0(fn, ".csv"))
  }
}

# set wd
setwd('C:\\Users\\Jerome\\Capstone')
trainfiles <- paste0(paste0(getwd(), "\\Train_Imgs\\"), list.files(path = paste0(getwd(), "\\Train_Imgs")))
testfiles <- paste0(paste0(getwd(), "\\Test_Imgs\\"), list.files(path = paste0(getwd(), "\\Test_Imgs")))
map1 <- paste0(paste0(getwd(), "\\Maps1_T\\"), list.files(path = paste0(getwd(), "\\Maps1_T")))
map2 <- paste0(paste0(getwd(), "\\Maps2_T\\"), list.files(path = paste0(getwd(), "\\Maps2_T")))
map3 <- paste0(paste0(getwd(), "\\Maps3_T\\"), list.files(path = paste0(getwd(), "\\Maps3_T")))
map4 <- paste0(paste0(getwd(), "\\Maps4_T\\"), list.files(path = paste0(getwd(), "\\Maps4_T")))
map5 <- paste0(paste0(getwd(), "\\Maps5_T\\"), list.files(path = paste0(getwd(), "\\Maps5_T")))
map6 <- paste0(paste0(getwd(), "\\Maps6_T\\"), list.files(path = paste0(getwd(), "\\Maps6_T")))


# simple exploration (map)
img = readImage(map1[1])
img <- resize(img, 500, 500)
str(img)
hist(img)

# simple exploration (train and test images which are the same type)
img = readImage(trainfiles[1])
img <- resize(img, 500, 500)
str(img)
hist(img)


# number of classes (what do they mean?)
res <- c()
for (i in 1:length(map1)) {
  unclean = readImage(map1[i])
  cleaned <- resize(unclean, 500, 500)
  cleaned <- round(cleaned*255)
  temp <- which.max(table(as.numeric(cleaned))[-1])
  res <- c(res, temp)
  print(sprintf("%s out of 244 done", i))
}



# Resize & and size new dimension pics
resizeImagesAndSave <- function(InputDrive, outputDrive, l, w) {
  files <- list.files(path = InputDrive)
  res <- list()
  for (i in 1:length(files)) {
    img <- readImage(paste0(InputDrive, "/", files[i]))
    img <- resize(img, l, w)
    writeImage(img, file = paste0(outputDrive, "/", files[i]))
    print(sprintf("%s ------ %s of out %s done", InputDrive, i, length(files)))
  }
  gc()
}

resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Maps1_T", "C:\\Users\\Jerome\\Capstone\\Maps1_T_resized", 500, 500)
resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Maps2_T", "C:\\Users\\Jerome\\Capstone\\Maps2_T_resized", 500, 500)
resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Maps3_T", "C:\\Users\\Jerome\\Capstone\\Maps3_T_resized", 500, 500)
resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Maps4_T", "C:\\Users\\Jerome\\Capstone\\Maps4_T_resized", 500, 500)
resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Maps5_T", "C:\\Users\\Jerome\\Capstone\\Maps5_T_resized", 500, 500)
resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Maps6_T", "C:\\Users\\Jerome\\Capstone\\Maps6_T_resized", 500, 500)
resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Train_imgs", "C:\\Users\\Jerome\\Capstone\\Train_imgs_resized", 500, 500)
resizeImagesAndSave("C:\\Users\\Jerome\\Capstone\\Test_imgs", "C:\\Users\\Jerome\\Capstone\\Test_imgs_resized", 500, 500)



## Modeling 
# model <- keras_model_sequential()
# model %>%
#   layer_conv_2d(filters = 32, 
#                 kernel_size = c(3,3),
#                 activation = 'relu',
#                 input_shape = c(100, 100, 3)) %>%
#   (filters = 32, kernel_size = c(3,3),
#       activation = 'relu') %>%
#   ---(pool_size = c(2,2)) %>%
#   ---(rate = 0.25) %>%
#   layer_conv_2d(filters = 64,
#                 kernel_size = c(3,3),
#                 activation = 'relu') %>%
#   layer_conv_2d(filters = 64,
#                 kernel_size = c(3,3),
#                 activation = 'relu') %>%
#   layer_max_pooling_2d(pool_size = c(2,2)) %>%
#   layer_dropout(rate = 0.25) %>%
#   ---() %>%
#   layer_dense(units = 256, activation = 'relu') %>%
#   layer_dropout(rate=0.25) %>%
#   layer_dense(units = 3, activation = '---') %>%
#   
#   compile(loss = '---',
#           optimizer = optimizer_sgd(lr = 0.01,
#                                     decay = 1e-6,
#                                     momentum = 0.9,
#                                     nesterov = T),
#           metrics = c('---'))
# summary(model)

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
