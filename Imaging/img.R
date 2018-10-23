getwd()
setwd("~/Desktop/BDSI/Imaging/training_set")

# load data 
# input an image as native raster
image_jpeg_nr <- readJPEG("ISIC_0000001.jpg", native=TRUE)
image_png_nr <- readPNG("ISIC_0000001_Segmentation.png", native=TRUE)
class(image_jpeg_nr)  #fromat: nativeRaser
dim(image_jpeg_nr) #height weight
head(image_jpeg_nr) #first colum
head(image_jpeg_nr[1:3,1:3]) #first 3 cols

# input as an array
image_jpeg_ar <- readJPEG("ISIC_0000001.jpg", native=FALSE)
image_png_ar <- readPNG("ISIC_0000001_Segmentation.png", native=FALSE)
dim(image_jpeg_ar) #height weight channels
head(image_jpeg_ar[,1:3,1]) # values are reals btwn 0 and 1

#read multiple images
pathway <- "~/Desktop/BDSI/Imaging/training_set"
file <- list.files(path=pathway,pattern=".jpg", all.files = TRUE, full.names = TRUE)
images_list <-lapply(X=file, FUN=readJPEG, native=TRUE)
#summary(images_list)
class(images_list[[1]])
dim(images_list[[1]])
images_list[[1]][1:3,1:3]

#raster image
a <- rep(c(0,1),length.out=15)
a.image <- matrix(a,ncol=5,nrow=3)
a.image <- as.raster(a.image)
a.image
plot(x=c(100,250),y=c(300,450),type="n") #generate a plot for image
rasterImage(image=a.image,xleft=100,ybottom=300,xright=250,ytop=450)
rasterImage(a.image,100,400,150,450, interpolate = FALSE) #set to FALSE for training
rasterImage(a.image,150,350,150+xinch(1.5),450+yinch(0.7))
rasterImage(a.image,150,350,200,450, angle=15)

#real skin lesion image
image_jpeg <- readJPEG("ISIC_0000001.jpg",native=TRUE)
plot(1:2,type="n",xlab="",ylab="")
rasterImage(image_jpeg,1,1,2,2)
# (b) Lesion segmentation image
image_png <- readPNG("ISIC_0000001_Segmentation.png",native=TRUE)
plot(1:2, type="n", xlab="", ylab="")
rasterImage(image_png, 1, 1, 2, 2)
# (c) Only lesion image
# Input image as array
image_jpeg <- readJPEG("ISIC_0000001.jpg", native=FALSE)
image_png <- readPNG("ISIC_0000001_Segmentation.png",native=FALSE)
# Replace the background '0' values into NA
is.na(image_png) <- image_png	== 0
# Masked image of the red component
masked <- image_png*image_jpeg[, , 1]
plot(1:2, type="n", xlab="", ylab="")
rasterImage(as.raster(masked), 1, 1, 2, 2)

##### Texture and features #####
# Texture Matrices
library(radiomics)
glcmDat <- glcm(masked, n_grey = 32) # gray level co-occurrence matrix
glrlmDat <- glrlm(masked) # gray level run length matrix
glszmDat <- glszm(masked) # gray level size zone matrix
mglszmDat <- mglszm(masked) # multiple gray level size zone matrix
image(glcmDat)
image(glrlmDat)

# Features
calc_features(masked)	# First order features
calc_features(glcmDat) # Grey Level Co-occurrence Matrix (GLCM) features
calc_features(glrlmDat)	# Grey Level Run Length Matrix (GLRLM) features
calc_features(glszmDat)	# Grey Level Size Zone Matrix (GLSZM) features
calc_features(mglszmDat)	# Multiple-GLSZM features




