#resizing function
library(EBImage)

# this function takes all the images in a folder, resizes them and copies them into a specified destination folder
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

# example
resizeImageAndSave(Original_Images_Folder_Path = "C:\\Users\\Jerome\\Desktop\\NUS Masters of Technology\\Capstone\\Maps1_T",
                   Destination_Images_Folder_Path = "C:\\Users\\Jerome\\Capstone\\test folder",
                   height_of_new_image = 500, 
                   width_of_new_image = 500)
