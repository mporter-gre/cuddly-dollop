
getPlane <- function(omConn, imageId, z, t, c){
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
  rVals$sizeX <- pixelsObj$getSizeX()
  rVals$sizeY <- pixelsObj$getSizeY()
  rVals$sizeZ <- pixelsObj$getSizeZ()
  rVals$sizeC <- pixelsObj$getSizeC()
  rVals$sizeT <- pixelsObj$getSizeT()
}