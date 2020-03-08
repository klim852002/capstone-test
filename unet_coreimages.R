library(unet)
library(keras)
library(tfdatasets)
library(tidyverse)
library(rsample)
library(EBImage)
library(reticulate)

maskpath <- "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\unettest\\masks"
trainpath <- "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\unettest\\train\\"

# mask = list.files(maskpath, full.names = T)
# for (i in 1:length(mask)) {
#   temp = readImage(mask[i]) 
#   temp = temp*255
#   writeImage(temp, files = mask[i])
#   
#   coreName = substr(list.files(maskpath), 1, 16)[i]
#   coreName2 <- paste0("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Train_Imgs\\", coreName, ".jpg")
#   temp2 <- readImage(coreName2)
#   writeImage(temp2, files = paste0(trainpath,coreName, ".jpg"))
#   print(sprintf("%s out of 244 done", i)) 
# }
 
# basic visualization initial examples
images <- tibble(
  img = list.files(trainpath, full.names = TRUE),
  mask = list.files(maskpath, full.names = TRUE)
) %>% 
  sample_n(2) %>% 
  map(. %>% magick::image_read() %>% magick::image_resize("128x128"))


# read files, split data into training and validation
data <- tibble(
  img = list.files(trainpath, full.names = TRUE),
  mask = list.files(maskpath, full.names = TRUE)
)

data <- initial_split(data, prop = 0.8)


# decode images into tensors, masks are saved as png, cores are saved as jpg (can use decode_jpeg for jpg files)
training_dataset <- training(data) %>%  
  tensor_slices_dataset() %>% 
  dataset_map(~.x %>% list_modify(
    img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
    mask = tf$image$decode_png(tf$io$read_file(.x$mask))
  ))

# The `[` calls wouldn't be necessary if `tf$image$decode_gif` returned a 3D Tensor like `tf$image$decode_jpeg` does. And if it could read just one color channel as we are only interested if it's black and white.
# If you are running this code interactively you can easily see the output of this
# chunk with:


example <- training_dataset %>% as_iterator() %>% iter_next()


# The above loaded the images into into a `uint8` Tensor. Which is great for reading 
# as it uses less memory. However for modelling we prefer having `float32` Tensors, 
# and that the values are in the [0,1] range. That's what we will fix now:

training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
    mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
  ))


training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = tf$image$resize(.x$img, size = shape(128, 128)),
    mask = tf$image$resize(.x$mask, size = shape(128, 128))
  ))


example <- training_dataset %>% as_iterator() %>% iter_next()
example$img %>% as.array() %>% as.raster() %>% plot()


random_bsh <- function(img) {
  img %>% 
    tf$image$random_brightness(max_delta = 0.3) %>% 
    tf$image$random_contrast(lower = 0.5, upper = 0.7) %>% 
    tf$image$random_saturation(lower = 0.5, upper = 0.7) %>% 
    tf$clip_by_value(0, 1) # clip the values into [0,1] range.
}

training_dataset <- training_dataset %>% 
  dataset_map(~.x %>% list_modify(
    img = random_bsh(.x$img)
  ))

example <- training_dataset %>% as_iterator() %>% iter_next()
example$img %>% as.array() %>% as.raster() %>% plot()


create_dataset <- function(data, train, batch_size = 32L) {
  dataset <- data %>% 
    tensor_slices_dataset() %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
      mask = tf$image$decode_gif(tf$io$read_file(.x$mask))[1,,,][,,1,drop=FALSE]
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
      mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$resize(.x$img, size = shape(128, 128)),
      mask = tf$image$resize(.x$mask, size = shape(128, 128))
    ))
  
  if (train) {
    dataset <- dataset %>% 
      dataset_map(~.x %>% list_modify(
        img = random_bsh(.x$img)
      )) 
  }
  
  if (train) {
    dataset <- dataset %>% 
      dataset_shuffle(buffer_size = batch_size*128)
  }
  
  dataset <- dataset %>% 
    dataset_batch(batch_size)
  
  
  
  dataset %>% 
    dataset_map(unname) # Keras needs an unnamed output.
}


training_dataset <- create_dataset(training(data), train = TRUE)
validation_dataset <- create_dataset(testing(data), train = FALSE)


model <- unet(input_shape = c(128, 128, 3))
summary(model)


model %>% compile(
  optimizer = optimizer_rmsprop(lr = 1e-5),
  loss = "binary_crossentropy",
  metrics = metric_binary_accuracy
)

model %>% fit(
  training_dataset,
  epochs = 500, 
  validation_data = validation_dataset
)

batch <- training_dataset %>% as_iterator() %>% iter_next()
predictions <- predict(model, batch)

images <- tibble(
  image = batch[[1]] %>% array_branch(1),
  predicted_mask = predictions[,,,1] %>% array_branch(1),
  mask = batch[[2]][,,,1]  %>% array_branch(1)
) %>% 
  sample_n(19) %>% 
  map_depth(2, function(x) {
    as.raster(x) %>% magick::image_read()
  }) %>% 
  map(~do.call(c, .x))



i = 1
plot(as_EBImage(images$image[i]))
plot(as_EBImage(images$mask[i]))
plot(as_EBImage(images$predicted_mask[i])) 
plot(ifelse(as_EBImage(images$predicted_mask[i])[,,1:3] < 0.5, 0, 1))




