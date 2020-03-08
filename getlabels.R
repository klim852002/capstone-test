library(EBImage)
library(data.table)
library(magrittr)
library(dplyr)

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
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

files <- c(paste0("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps1_T\\", list.files(path="C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps1_T")), 
           paste0("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps2_T\\", list.files(path="C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps2_T")), 
           paste0("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps3_T\\", list.files(path="C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps3_T")), 
           paste0("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps4_T\\", list.files(path="C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps4_T")), 
           paste0("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps5_T\\", list.files(path="C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps5_T")), 
           paste0("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps6_T\\", list.files(path="C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps6_T")))

## get labels for each map
# num <- seq(from = 1, to = 1180, by = 20)
# res <- data.table()
# for (i in 1:length(files)) {
#   img = readImage(files[i])
#   img255mode <- img*255
#   result <- table(img255mode)
#   label_1 <- names(sort(result, decreasing = T))[1]
#   label_2 <- names(sort(result, decreasing = T))[2]
#   temp <- data.table(coreName = files[i] ,label_1 = as.numeric(label_1), label_2 = as.numeric(label_2))
#   res <- rbind(res, temp)
#   if (i %in% num) save(res, file = "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\LabelledMaps.RData")
#   gc()
#   print(sprintf("%s out of 1182 done", i))
# }
#save(res, file = "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\LabelledMaps.RData")

load("C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\LabelledMaps.RData")
res$coreNameFinal <- unlist(lapply(strsplit(res$coreName, split = "\\\\"), function(x) x[8]))
res %<>% select(coreNameFinal, label_1, label_2) %>% arrange(coreNameFinal) %>% as.data.table

res$label_1 <- ifelse(res$label_1 %in% c(0,1,6), 0, res$label_1)
res$label_2 <- ifelse(res$label_2 %in% c(0,1,6), 0, res$label_2)
######## need to ask prof about this step
res$final <- apply(res[, c(2,3)], 1, function(x) max(x, na.rm = T)) 
res2 <- res %>% group_by(coreNameFinal) %>% summarise(final_label = getmode(final), num_pathologist_labelled = n()) %>% as.data.table
table(res2$final_label)

save(res2, file = "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\LabelledMapsUsingMax_ThenTakeMode.RData")


# resize all train images to 5000x5000
resizeImageAndSave <- function(Original_Images_Folder_Path, Destination_Images_Folder_Path, height_of_new_image, width_of_new_image) {
  files <- list.files(path = Original_Images_Folder_Path)
  print(sprintf("%s files found in -- %s --", length(files), Original_Images_Folder_Path))
  for (i in 1:length(files)) {
    file <- paste0(Original_Images_Folder_Path, "\\", files[i])
    img <- readImage(file)  
    resized <- resize(img, width_of_new_image, height_of_new_image)
    resizedImage <- paste0(Destination_Images_Folder_Path, "\\", files[i])
    writeImage(resized, files = resizedImage)
    print(sprintf("%s out of %s done", i, length(files)))
  }
}

resizeImageAndSave(Original_Images_Folder_Path = "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps1_T",
                   Destination_Images_Folder_Path = "C:\\Users\\Jerome\\Capstone\\test folder",
                   height_of_new_image = 500, 
                   width_of_new_image = 500)


# match train images names with labels data
load( "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\LabelledMapsUsingMax_ThenTakeMode.RData")
train_imgs_path = "C:\\Users\\Jerome\\Capstone\\Train_imgs"

train_imgs <- data.table(train_imgs_names = list.files(path = train_imgs_path))
train_imgs$train_imgs_names2 <- substr(train_imgs$train_imgs_names, 1, 16)
res2$coreNameFinal <- substr(res2$coreNameFinal, 1, 16)
combined <- merge(train_imgs, res2, by.x = "train_imgs_names2", by.y = "coreNameFinal")
combined %<>% select(-train_imgs_names2)
combined$train_imgs_names <- paste0(train_imgs_path, combined$train_imgs_names)

