#'InterpolateVector
#'@description Interplolates a vector
#'@export
#'@importFrom stats approx
#'@param age Vector of ages from the date.
#'@param data Vector of one parameter.
#'@param thick thickness of ages to interpolate to.
#'@return Return an interpolatet vector of the given date.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

InterpolateVector = function(age,data,thick=50){

  whileLoop=T
  i=0
  while(whileLoop){

    i=i+1

    if(!is.na(data[i])){

      min=ceiling(age[i]/thick)*thick
      whileLoop=F

    }
  }

  whileLoop=T
  i=length(data)+1
  while(whileLoop){

    i=i-1

    if(!is.na(data[i])){

      max=floor(age[i]/thick)*thick
      whileLoop=F

    }
  }

  InterploationSeqence=seq(from = min, to = max, by = thick)

  InterApprox=approx(age, data, xout = InterploationSeqence, method = "linear",na.rm=T)

  return(InterApprox)

}
