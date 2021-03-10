#'AddIntervalsToFolder
#'@description Adds intervalls to a folder structure with a depth on 1.
#'@export
#'@param ListCorrelation folder structure with a depth on 1.
#'@param intervall percentage distance to the apex in which all data are to be included.
#'@return Returns the folder structure with intervals addet.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

AddIntervalsToFolder = function(ListCorrelation,intervall=0.05){

  for(i in 1:length(ls(ListCorrelation))){

    LimitValue=max(abs(ListCorrelation[[i]][[2]]),abs(ListCorrelation[[i]][[3]]))

    Distance=abs(ListCorrelation[[i]][[2]]-ListCorrelation[[i]][[3]])

    DistanceToTop=Distance*intervall

    k=0
    z=0
    value=NA
    age=NA

    for (j in ListCorrelation[[i]][[1]]){

      k=k+1
      z=z+1

      if (abs(j)>(LimitValue-DistanceToTop) && abs(j)<(LimitValue+DistanceToTop)){

        value[k]=j
        age[k]=as.numeric(names(ListCorrelation[[i]][[1]][z]))

      }else{

        k=k-1

      }
    }

    value=mean(value)
    names(value)=mean(age)

    ListCorrelation[[i]][["intervall"]]=value

  }

  return(ListCorrelation)

}
