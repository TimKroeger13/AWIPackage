#'Resample
#'@description Resample a List of Data to an origin data set.
#'@export
#'@import rlist
#'@param SourceData Dataframe after the other dataframes are to be resampled.
#'@param Datalist A list of dataframes to be resampled.
#'@return Resample retruns a List of all all resampled dataframes as a List.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

Resample = function(SourceData,Datalist){

  out=list()

  for (i in 1:length(ls(Datalist))){

    tempdata=Datalist[[i]]

    k=0

    while (k<length(SourceData[,1])){

      k=k+1

      if(is.na(match(SourceData[k,1],tempdata[,1]))){

        SourceData=SourceData[-k,]

        k=k-1

      }
    }

    tempdataResults=matrix(NA,nrow = dim(SourceData)[1],ncol = dim(tempdata)[2])

    for (k in 1:dim(SourceData)[1]){

      tempdataResults[k,]=as.numeric(tempdata[match(SourceData[k,1],tempdata[,1]),])

    }

    tempdataResults=as.data.frame(tempdataResults)
    colnames(tempdataResults)=colnames(tempdata)

    out=list.append(out,tempdataResults)

}

  names(out)=ls(Datalist)

  return(out)

}
