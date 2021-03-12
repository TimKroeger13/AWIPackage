#'PlotFolder
#'@description Plots data from Folder Structure.
#'@export
#'@importFrom graphics par
#'@importFrom stats na.omit
#'@param data List of named data or a List List of named data.
#'@param name Name for the Plot when one the direct data was used..
#'@return Plots data.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

PlotFolder=function(data,name="Set_main"){

  Listdepth <- function(this,thisdepth=0){
    if(!is.list(this)){
      return(thisdepth)
    }else{
      return(max(unlist(lapply(this,Listdepth,thisdepth=thisdepth+1))))
    }
  }

  if(Listdepth(data)==0){

    par(mfrow=c(1,1))

    plot(NA,
         xlim=c(min(na.omit(data)),max(na.omit(data))),
         ylim=c(max(na.omit(as.numeric(names(data)))),min(na.omit(as.numeric(names(data))))),
         ylab="Age",
         xlab="value",
         main=name
    )

    lines(na.omit(cbind(data,as.numeric(names(data)))),type = "l")

  }else if(Listdepth(data)==1){

    par(mfrow=c(1,length(ls(data))))

    MinForAll=NULL
    MaxForAll=NULL

    for (i in 1:length(ls(data))){

      MinForAll[i]=min(as.numeric(names(na.omit(data[[i]]))))
      MaxForAll[i]=max(as.numeric(names(na.omit(data[[i]]))))

    }

    for (i in 1:length(ls(data))){

      plot(NA,
           xlim=c(min(na.omit(data[[i]])),max(na.omit(data[[i]]))),
           ylim=c(max(MaxForAll),min(MinForAll)),
           ylab="Age",
           xlab="value",
           main=names(data[i])
      )

      lines(na.omit(cbind(data[[i]],as.numeric(names(data[[i]])))),type = "l")

    }

    par(mfrow=c(1,1))

  }
}
