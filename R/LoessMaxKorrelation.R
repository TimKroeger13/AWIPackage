#'LoessMaxKorrelation
#'@description Calculates all Max and Min values for an Folder structure with the Loess function.
#'@export
#'@param main Vector from FolderCreater.
#'@param folder Folder structur to calculate all values from.
#'@param thick Thickness of age intervals to interpolate to.
#'@param offset Size of the area in which all correlations are calculated. must be dividable by the thickness!
#'@param IgnorePercent Change this values carefully. Specifies a minimum percentage of the read data that should not be NA.
#'@return Returns a Dataframe with all Min and Max value for each element of the FOlder structure.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

LoessMaxKorrelation = function(main,folder,thick,offset,IgnorePercent=0.1){

  TempFolder=list()
  TempUnderfolder=list()

  for(i in 1:length(ls(folder))){

    TempNames=names(folder[[i]])
    MainNames=names(folder)
    TempUnderfolder=list()

    for(k in 1:length(ls(folder[[i]]))){

      if((sum(folder[[i]][[k]][[2]]>0,na.rm = T))>(length(folder[[i]][[k]][[2]])*IgnorePercent)){

        TempUnderfolder[[TempNames[k]]]=folder[[i]][[k]][[2]]

      }
    }

    TempFolder[[MainNames[i]]]=TempUnderfolder

  }

  CorvaluesMax=NULL
  CorvaluesMin=NULL
  CorNames=NULL

  for(i in 1:length(ls(TempFolder))){

    correlation=ListCorrelation(main = main,dataAsList = TempFolder[[i]],thick = thick,offset = offset)

    TempNames=names(TempFolder[[i]])

    for(k in 1:length(ls(TempFolder[[i]]))){

      CorvaluesMax[length(CorvaluesMax)+1]=correlation[[k]][2]
      CorvaluesMin[length(CorvaluesMin)+1]=correlation[[k]][3]
      CorNames[length(CorNames)+1]=TempNames[[k]]

    }
  }

  output=cbind(as.character(CorNames),as.numeric(CorvaluesMax),as.numeric(CorvaluesMin))

  colnames(output)=c("names","max","min")

  return(output)

}
