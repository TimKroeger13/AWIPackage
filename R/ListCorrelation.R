#'ListCorrelation
#'@description Calculates with two vectors all correlations with time lag.
#'@export
#'@param main Vector from FolderCreater.
#'@param dataAsList List of Value from FolderCreater.
#'@param thick Thickness of age intervals to interpolate to.
#'@param offset Size of the area in which all correlations are calculated. must be dividable by the thickness!
#'@return Returns a list of all correlations in the offset area.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

ListCorrelation=function(main,dataAsList,thick=50,offset=2000){

  TempNames=names(dataAsList)

  folder=list()

  for(i in 1:length(ls(dataAsList))){

    TempData=dataAsList[[i]]

    TempData=VectorCorrelationAll(x=c(as.numeric(names(TempData)),TempData),
                                  y=c(as.numeric(names(main)),main),
                                  thick = thick,
                                  offset = offset)

    folder[[TempNames[i]]]=TempData

  }

  return(folder)

}
