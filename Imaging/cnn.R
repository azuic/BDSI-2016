img.resize <- lapply(img.crop.row,FUN=resize,w=64,h=64)
test.resize <- lapply(testcrop.r,FUN=resize,w=64,h=64)

try <- img.resize
try.mtx <- try@.Data
try.vec <- as.vector(t(try))
