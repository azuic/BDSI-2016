
###### Library #####
library(jpeg)
library(png)
library(radiomics)


##### Load Data #####

# f <- function(i,j){
#   files <- list.files(path=yourpath, pattern=".jpg", all.files=TRUE, full.names=TRUE)
#   images_list <- lapply(X=files[i:j], FUN=readJPEG, native=FALSE)
# }

###### Read multiple images
yourpath <- "~/Desktop/BDSI/Imaging/training_set"
files <- list.files(path=yourpath, pattern=".jpg", all.files=TRUE, full.names=TRUE)

images_list_1_50 <- lapply(X=files[1:50], FUN=readJPEG, native=FALSE)
images_list_51_100 <- lapply(X=files[51:100], FUN=readJPEG, native=FALSE)
images_list_101_150 <- lapply(X=files[101:150], FUN=readJPEG, native=FALSE)
images_list_151_200 <- lapply(X=files[151:200], FUN=readJPEG, native=FALSE)
images_list_201_250 <- lapply(X=files[201:250], FUN=readJPEG, native=FALSE)

images_list_251_500 <- lapply(X=files[251:500], FUN=readJPEG, native=FALSE)
images_list_501_700 <- lapply(X=files[501:700], FUN=readJPEG, native=FALSE)

###### Input a list of segmentation images
seg_files <- list.files(path = yourpath, pattern = ".png", full.names = TRUE)

seg_list_1_50 <- lapply(X=seg_files[1:50], FUN=readPNG, native=FALSE)
seg_list_51_100 <- lapply(X=seg_files[51:100], FUN=readPNG, native=FALSE)
seg_list_101_150 <- lapply(X=seg_files[101:150], FUN=readPNG, native=FALSE)
seg_list_151_200 <- lapply(X=seg_files[151:200], FUN=readPNG, native=FALSE)
seg_list_201_250 <- lapply(X=seg_files[201:250], FUN=readPNG, native=FALSE)

seg_list_251_500 <- lapply(X=seg_files[251:500], FUN=readPNG, native=FALSE)
seg_list_501_700 <- lapply(X=seg_files[501:700], FUN=readPNG, native=FALSE)


###### Define a function to convert a single image to gray-scale image
gray_scale <- function(inputImage){
  outputImage <- inputImage[,,1]*0.21 + inputImage[,,2]*0.72 + inputImage[,,3]*0.07
}
# temp <- gray_scale(images_list[[1]])
# dim(temp)
# plot(0:1, 0:1, type="n")
# rasterImage(temp, 0, 0, 1, 1)

###### Use lapply() to process a list 
outImage_1_50 <- lapply(images_list_1_50, FUN=gray_scale)
outImage_51_100 <- lapply(images_list_51_100, FUN=gray_scale)
outImage_101_150 <- lapply(images_list_101_150, FUN=gray_scale)
outImage_151_200 <- lapply(images_list_151_200, FUN=gray_scale)
outImage_201_250 <- lapply(images_list_201_250, FUN=gray_scale)

outImage_251_500 <- lapply(images_list_251_500, FUN=gray_scale)
outImage_501_700 <- lapply(images_list_501_700, FUN=gray_scale)

# class(outImage)
# length(outImage)
# length(outImage_10)
# dim(outImage[[1]])
# plot(0:1, 0:1, type="n")
# rasterImage(outImage[[1]], 0, 0, 1, 1)
# rasterImage(outImage[[6]], 0, 0, 1, 1)

# Define a function to remove background
lesion <- function(inputImage, inputSeg){
  is.na(inputSeg) <- inputSeg == 0
  lesionImage <- inputImage*inputSeg
}



outlesions_1_50 <- lapply(1:50, function(i) lesion(outImage_1_50[[i]],seg_list_1_50[[i]]))
outlesions_51_100 <- lapply(1:50, function(i) lesion(outImage_51_100[[i]],seg_list_51_100[[i]]))
outlesions_101_150 <- lapply(1:50, function(i) lesion(outImage_101_150[[i]],seg_list_101_150[[i]]))
outlesions_151_200 <- lapply(1:50, function(i) lesion(outImage_151_200[[i]],seg_list_151_200[[i]]))
outlesions_201_250 <- lapply(1:50, function(i) lesion(outImage_201_250[[i]],seg_list_201_250[[i]]))

outlesions_251_500 <- lapply(1:250, function(i) lesion(outImage_251_500[[i]],seg_list_251_500[[i]]))
outlesions_501_700 <- lapply(1:200, function(i) lesion(outImage_501_700[[i]],seg_list_501_700[[i]]))

# length(outlesions)
# length(outlesions_10)

# class(outlesions)
# dim(outlesions[[1]])
# par(mfrow=c(1,3))
# plot(0:1,0:1,"n")
# rasterImage(outlesions[[1]],0,0,1,1)
# plot(0:1,0:1,"n")
# rasterImage(outlesions[[2]],0,0,1,1)
# plot(0:1,0:1,"n")
# rasterImage(outlesions[[3]],0,0,1,1)
# par(mfrow=c(1,1))


# save workspace
save(outlesions_1_50, file="outlesions_1_50.RData")
save(outlesions_51_100, file="outlesions_51_100.RData")
save(outlesions_101_150, file="outlesions_101_150.RData")
save(outlesions_151_200, file="outlesions_151_200.RData")
save(outlesions_201_250, file="outlesions_201_250.RData")

# load("outlesions_10.RData")

# 1-50
#create features matrices#
glcmDat_1_50 <- lapply(outlesions_1_50, FUN=glcm,n_grey=32)
glrlmDat_1_50 <- lapply(outlesions_1_50, FUN=glrlm)
glszmDat_1_50 <- lapply(outlesions_1_50, FUN=glszm)
mglszmDat_1_50 <- lapply(outlesions_1_50, FUN=mglszm)

# Create Features from matrics#
finaldat_1_50 <- t(rbind(sapply(outlesions_1_50,FUN=calc_features),	# First order features
                   sapply(glcmDat_1_50,FUN=calc_features), # Grey Level Co-occurrence Matrix (GLCM) features
                   sapply(glrlmDat_1_50,FUN=calc_features),	# Grey Level Run Length Matrix (GLRLM) features
                   sapply(glszmDat_1_50,FUN=calc_features),	# Grey Level Size Zone Matrix (GLSZM) features
                   sapply(mglszmDat_1_50,FUN=calc_features)))	# Multiple-GLSZM features

write.csv(finaldat_1_50,'imagingfeatures_1_50.csv')

# 51-100
#create features matrices#
glcmDat_51_100 <- lapply(outlesions_51_100, FUN=glcm,n_grey=32)
glrlmDat_51_100 <- lapply(outlesions_51_100, FUN=glrlm)
glszmDat_51_100 <- lapply(outlesions_51_100, FUN=glszm)
mglszmDat_51_100 <- lapply(outlesions_51_100, FUN=mglszm)

# Create Features from matrics#
finaldat_51_100 <- t(rbind(sapply(outlesions_51_100,FUN=calc_features),	# First order features
                         sapply(glcmDat_51_100,FUN=calc_features), # Grey Level Co-occurrence Matrix (GLCM) features
                         sapply(glrlmDat_51_100,FUN=calc_features),	# Grey Level Run Length Matrix (GLRLM) features
                         sapply(glszmDat_51_100,FUN=calc_features),	# Grey Level Size Zone Matrix (GLSZM) features
                         sapply(mglszmDat_51_100,FUN=calc_features)))	# Multiple-GLSZM features

write.csv(finaldat_51_100,'imagingfeatures_51_100.csv')

# 101-150
#create features matrices#
glcmDat_101_150 <- lapply(outlesions_101_150, FUN=glcm,n_grey=32)
glrlmDat_101_150 <- lapply(outlesions_101_150, FUN=glrlm)
glszmDat_101_150 <- lapply(outlesions_101_150, FUN=glszm)
mglszmDat_101_150 <- lapply(outlesions_101_150, FUN=mglszm)

# Create Features from matrics#
finaldat_101_150 <- t(rbind(sapply(outlesions_101_150,FUN=calc_features),	# First order features
                         sapply(glcmDat_101_150,FUN=calc_features), # Grey Level Co-occurrence Matrix (GLCM) features
                         sapply(glrlmDat_101_150,FUN=calc_features),	# Grey Level Run Length Matrix (GLRLM) features
                         sapply(glszmDat_101_150,FUN=calc_features),	# Grey Level Size Zone Matrix (GLSZM) features
                         sapply(mglszmDat_101_150,FUN=calc_features)))	# Multiple-GLSZM features

write.csv(finaldat_101_150,'imagingfeatures_101_150.csv')

# 151-200
#create features matrices#
glcmDat_151_200 <- lapply(outlesions_151_200, FUN=glcm,n_grey=32)
glrlmDat_151_200 <- lapply(outlesions_151_200, FUN=glrlm)
glszmDat_151_200 <- lapply(outlesions_151_200, FUN=glszm)
mglszmDat_151_200 <- lapply(outlesions_151_200, FUN=mglszm)

# Create Features from matrics#
finaldat_151_200 <- t(rbind(sapply(outlesions_151_200,FUN=calc_features),	# First order features
                         sapply(glcmDat_151_200,FUN=calc_features), # Grey Level Co-occurrence Matrix (GLCM) features
                         sapply(glrlmDat_151_200,FUN=calc_features),	# Grey Level Run Length Matrix (GLRLM) features
                         sapply(glszmDat_151_200,FUN=calc_features),	# Grey Level Size Zone Matrix (GLSZM) features
                         sapply(mglszmDat_151_200,FUN=calc_features)))	# Multiple-GLSZM features

write.csv(finaldat_151_200,'imagingfeatures_151_200.csv')

# 201-250
#create features matrices#
glcmDat_201_250 <- lapply(outlesions_201_250, FUN=glcm,n_grey=32)
glrlmDat_201_250 <- lapply(outlesions_201_250, FUN=glrlm)
glszmDat_201_250 <- lapply(outlesions_201_250, FUN=glszm)
mglszmDat_201_250 <- lapply(outlesions_201_250, FUN=mglszm)

# Create Features from matrics#
finaldat_201_250 <- t(rbind(sapply(outlesions_201_250,FUN=calc_features),	# First order features
                         sapply(glcmDat_201_250,FUN=calc_features), # Grey Level Co-occurrence Matrix (GLCM) features
                         sapply(glrlmDat_201_250,FUN=calc_features),	# Grey Level Run Length Matrix (GLRLM) features
                         sapply(glszmDat_201_250,FUN=calc_features),	# Grey Level Size Zone Matrix (GLSZM) features
                         sapply(mglszmDat_201_250,FUN=calc_features)))	# Multiple-GLSZM features

write.csv(finaldat_1_50,'imagingfeatures_201_250.csv')


##########################################################################################################
##########################################################################################################
##########################################################################################################


