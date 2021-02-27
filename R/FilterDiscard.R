#'FilterDiscard
#'@description Checks how much data would be lost due to filtering.
#'@export
#'@param data Dataframe from LoadMultivar.
#'@param filter Vector of 2 Integer. First Number stands for min Precent. Second Number stands for minimum number of rows with "min percent".
#'@return Returns a Dataframe.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

FilterDiscard <- function(data,filter = c(0,0)){

  Back=data[,3:dim(data)[2]]

  PreNASum=sum(is.na(Back))

  for (i in 1:dim(Back)[2]){

    if(sum(Back[,i]>=filter[1],na.rm = T)<filter[2]){

      Back[,i]=NA

    }
  }

  FilterPercent=round((sum(is.na(Back))-PreNASum)/(dim(Back)[1]*dim(Back)[2])*100,digits = 2)

  return(FilterPercent)

}
