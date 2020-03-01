mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)


model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 32,
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(100, 100, 3)) %>%
  layer_conv_2d(filters = 32, 
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_conv_2d(filters = 64,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate=0.25) %>%
  layer_dense(units = 3, activation = 'softmax') %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_sgd(lr = 0.01,
                                    decay = 1e-6,
                                    momentum = 0.9,
                                    nesterov = T),
          metrics = c('accuracy'))

summary(model)


model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 10,
                kernel_size = 3,
                activation = 'relu', 
                input_shape = c(28, 28)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  compile(loss = 'MSE',
          optimizer = optimizer_rmsprop())

summary(model)

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)





############################################################################
library(keras)
library(caret)
library(data.table)
#cifar <- dataset_cifar10()

train_data <- scale(cifar$train$x)
dim(train_data) <- c(50000,32,32,3)

test_data <- scale(cifar$test$x)
dim(test_data) <- c(10000,32,32,3)

train_label <- as.numeric(cifar$train$y)
dim(train_label) <- c(50000)

test_label <- as.numeric(cifar$test$y)
dim(test_label) <- c(10000)

#######################################################################

class_names <- c('airplane', 'automobile', 'bird', 'cat', 'deer',
                 'dog', 'frog', 'horse', 'ship', 'truck')

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu", input_shape = c(32,32,3)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_flatten() %>% 
  compile(optimizer = "adam", 
          loss = "sparse_categorical_crossentropy",
          metrics = "accuracy") 

history <-  model %>% 
  fit(train_data, train_label,
  validation_split = 0.2, 
  epoch = 30, batch_size = 128)

summary(model)

output <- predict(model, train_data[1:10000,,,])
pca <- prcomp(output, center = T)
dim(pca$x)

summaryPCA <- summary(pca)
plot <- data.table(raw = summaryPCA$sdev^2, sum= cumsum(summaryPCA$sdev^2))
plot$pct <- plot$sum/max(plot$sum)
ggplot(plot) + geom_line(aes(x = 1:2304, y = raw), color = COLO) + 
  

for (i in 1:1000) {
  tempModel <- 
}


