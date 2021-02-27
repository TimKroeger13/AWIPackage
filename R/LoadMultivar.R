#'LoadMultivar
#'@description Loads Diatom data for Multivariate statistics
#'@export
#'@importFrom utils read.csv read.table
#'@param data Name of the dataset in CSV format.
#'@param sep Separator in the CSV.
#'@param DataAsPercent Bool indicating whether data should be convertet to percent or not.
#'@param SartOfData Number indicating the Collum where the Diatom data stats.
#'@param Comparison Number indicating the Collum where the Depth or Age is located.
#'@return Returns a Dataframe.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

LoadMultivar = function(data, sep=",", DataAsPercent = T, SartOfData = 4, Comparison = 2){

  if(!substr(data, nchar(data)-3, nchar(data))==".csv"){

    data=paste(data,".csv",sep = "")

  }

  data=read.csv(data,header=T,sep = sep, fileEncoding = "latin1")

  if(dim(data)[2]==1){

    stop("Data was loaded incorrectly. Check sep = ...")

  }

  DataToPercent <- function(data) {

    for(i in 1:dim(data)[1]){

      data[i,]=round(data[i,]/sum(data[i,],na.rm = T)*100,digits = 2)

    }

    return(data)
  }

  site = data[,1]
  comparisonAxis = data[,Comparison]
  data = data[,SartOfData:dim(data)[2]]

  if(DataAsPercent){

    data=DataToPercent(data)

  }

  data=cbind(site,comparisonAxis,data)

  return(data)

}
