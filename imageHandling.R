
getPlane <- function(omConn, imageId, z = 1, t, c){
  imgObj <- loadObject(omConn, 'ImageData', imageId)
  plane <- getPixelValues(imgObj, z, t, c)
}

getStack <- function(omConn, imageId, t, c){
  imgObj <- loadObject(omConn, 'ImageData', imageId)
  pixelsObj <- imgObj@dataobject$getDefaultPixels()
  numZ <- pixelsObj$getSizeZ()
}

getImageDimensions <- function(imgObj){
  pixelsObj <- imgObj@dataobject$getDefaultPixels()
  imgDims <- c(pixelsObj$getSizeX(), pixelsObj$getSizeY(), pixelsObj$getSizeZ(), pixelsObj$getSizeC(), pixelsObj$getSizeT())
}

