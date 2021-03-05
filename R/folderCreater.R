#'FolderCreater
#'@description Assembles lists of data tables into a manageable folder sythem.
#'@export
#'@param data Data sheets in with ages and values to be sorted in.
#'@param format Information about the format of the data sheets:\cr\cr
#'If format="collective" it is expected that the first column contains an age and all other columns contain the corresponding values.\cr\cr
#'If format="segment" it is expected that there are always alternating ages and values next to each other.\cr\cr
#'Starting with the age.\cr
#'Example: Age...V1...Age...V2...
#'@return Returns a folder structure based on lists.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

FolderCreater=function(data,format="collective"){

  folder=list()

  if(is.list(data)){

    if(format=="collective"){

      for(i in 1:length(ls(data))){

        TempMainName=List[i]

        TempData=data[[i]]

        TempNames=(colnames(TempData)[2:length(colnames(TempData))])

        TempDepth=TempData[,1]

        for (k in 1:length(TempNames)){

          PrepDataForFolder=TempData[,k+1]
          names(PrepDataForFolder)=TempDepth

          folder[[TempMainName]][[TempNames[k]]]=PrepDataForFolder

        }
      }
    }

    if(format=="segment"){

      i=-1
      k=NA

      TempNames=colnames(data)

      while (i<length(ls(data))-1){

        i=i+2
        k=i+1

        PrepDataForFolder=data[,k]
        names(PrepDataForFolder)=data[,i]

        folder[[TempNames[k]]]=PrepDataForFolder

      }
    }
  }

  return(folder)

}
