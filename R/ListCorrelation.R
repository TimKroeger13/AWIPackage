#'ListCorrelation
#'@description Calculates with two vectors all correlations with time lag.
#'@export
#'@param Main Vector that consists of an age and data vector. c(Age,Vector).
#'@param List List of Value from FolderCreater.
#'@param thick Thickness of age intervals to interpolate to.
#'@param offset Size of the area in which all correlations are calculated. must be dividable by the thickness!
#'@return Returns a list of all correlations in the offset area.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

ListCorrelation=function(Main,List,thick=50,offset=2000){

  TempNames=names(List)

  folder=list()

  for(i in 1:length(ls(List))){

    main1=Main[1:(length(Main)/2)]
    main2=Main[((length(Main)/2)+1):length(Main)]

    TempData=List[[i]]

    TempData=VectorCorrelationAll(x=c(as.numeric(names(TempData)),TempData),
                                  y=c(main1,main2),
                                  thick = thick,
                                  offset = offset)

    folder[[TempNames[i]]]=TempData

  }

  return(folder)

}
