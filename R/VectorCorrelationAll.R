#'VectorCorrelationAll
#'@description Calculates with two vectors all correlations with time lag.
#'@export
#'@param x Vector that consists of an age and data vector. c(Age,Vector).
#'@param y Vector that consists of an age and data vector. c(Age,Vector).
#'@param thick Thickness of age intervals to interpolate to.
#'@param offset Size of the area in which all correlations are calculated. must be dividable by the thickness!
#'@return Returns a list of all correlations in the offset area.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

VectorCorrelationAll = function(x,y,thick=50,offset=2000){

  x1=x[1:(length(x)/2)]
  x2=x[((length(x)/2)+1):length(x)]
  y1=y[1:(length(y)/2)]
  y2=y[((length(y)/2)+1):length(y)]

  Faktor1=InterpolateVector(age = x1,data = x2,thick = thick)
  Faktor2=InterpolateVector(age = y1,data = y2,thick = thick)

  TempVector=array(NA,dim=(offset/thick)*2)
  NameVecor=array(NA,dim=(offset/thick)*2)

  movingOffset=0-offset-thick

  for (i in 1:(((offset/thick)*2)+1)){

    movingOffset=movingOffset+thick

    TempVector[i]=VectorCorrelation(x = Faktor1,y = Faktor2,offset = movingOffset)
    NameVecor[i]=movingOffset

  }

  out=list()

  names(TempVector)=NameVecor
  TempMin=min(TempVector)
  names(TempMin)=NameVecor[which(TempVector %in% TempMin)]
  TempMax=max(TempVector)
  names(TempMax)=NameVecor[which(TempVector %in% TempMax)]

  out$main=TempVector
  out$max=TempMax
  out$min=TempMin

  return(out)

}
