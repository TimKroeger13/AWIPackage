#'VectorCorrelation
#'@description Correlation of two vectors with optional offset.
#'@export
#'@param x List from the function VectorCorrelation.
#'@param y List from the function VectorCorrelation.
#'@param offset Offset of ages. Must be in limits of the interpolation.
#'@return Retruns the r value from the correlation.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

VectorCorrelation = function(x,y,offset=0){

  x$x=x$x+offset

  distance=which(x$x==min(max(x$x),max(y$x)))-which(x$x==max(min(x$x),min(y$x)))

  CorAges=x$x[which(x$x==max(min(x$x),min(y$x))):which(x$x==min(max(x$x),max(y$x)))]

  factorX=x$y[x$x %in% CorAges]
  factorY=y$y[y$x %in% CorAges]

  return(cor(factorX,factorY))

}
