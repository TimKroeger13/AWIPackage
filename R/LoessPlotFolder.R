#'LoessPlotFolder
#'@description Plots data from Folder Structure with the Loess function.
#'@export
#'@importFrom graphics par
#'@importFrom stats na.omit
#'@importFrom graphics axis
#'@param data List of named data or a List List of named data.
#'@param alpha size of the confidence intervals.
#'@param span the parameter α which controls the degree of smoothing.
#'@param name Name for the Plot when one the direct data was used.
#'@param custom_age custom y Achses max age.
#'@return Plots data.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

LoessPlotFolder=function(data,alpha = 0.05,span = 0.2,name="Set_main",custom_age=NA){

  Listdepth <- function(this,thisdepth=0){
    if(!is.list(this)){
      return(thisdepth)
    }else{
      return(max(unlist(lapply(this,Listdepth,thisdepth=thisdepth+1))))
    }
  }

  if(Listdepth(data)==0){

    par(mfrow=c(1,1))

    LineData=LoessFromData(data,alpha = alpha,span = span)

    if(is.na(custom_age)){

    plot(NA,
         xlim=c(min(min(na.omit(data)), min(na.omit(LineData$LoessConfDown))),
                max(max(na.omit(data)),max(na.omit(LineData$LoessConfUp)))),
         ylim=c(max(na.omit(as.numeric(names(data)))),min(na.omit(as.numeric(names(data))))),
         ylab="Age",
         xlab="value",
         main=paste(name,span)
    )

      customlabels=seq(from=0,
          to =ceiling(max(na.omit(as.numeric(names(data))))/1000)*1000,
          by =1000)

    axis(2,at=customlabels,labels = character(length(customlabels)))

    }else{

      plot(NA,
           xlim=c(min(min(na.omit(data)), min(na.omit(LineData$LoessConfDown))),
                  max(max(na.omit(data)),max(na.omit(LineData$LoessConfUp)))),
           ylim=c(custom_age,min(na.omit(as.numeric(names(data))))),
           ylab="Age",
           xlab="value",
           main=paste(name,span)
      )

      customlabels=seq(from=0,
                       to =ceiling(custom_age/1000)*1000,
                       by =1000)

      axis(2,at=customlabels,labels = character(length(customlabels)))

    }

    points(na.omit(cbind(data,as.numeric(names(data)))))

    lines(as.numeric(names(LineData$LoessMean)) ~ LineData$LoessMean)
    lines(as.numeric(names(LineData$LoessConfUp)) ~ LineData$LoessConfUp,lty=3)
    lines(as.numeric(names(LineData$LoessConfDown)) ~ LineData$LoessConfDown,lty=3)

  }else if(Listdepth(data)==2){

    if(length(ls(data))>6){

      par(mfrow=c(1,6))

    }else{

      par(mfrow=c(1,length(ls(data))))

    }

    MinForAll=NULL
    MaxForAll=NULL

    for (i in 1:length(ls(data))){

      MinForAll[i]=min(as.numeric(names(na.omit(data[[i]][[1]]))))
      MaxForAll[i]=max(as.numeric(names(na.omit(data[[i]][[1]]))))

    }

    VariablesToPlot=length(ls(data))
    VariablesToPlotPerLoop=0
    VariableCounter=0

    for(z in 1:ceiling(length(ls(data))/6)){

      if(VariablesToPlot>6){

        VariablesToPlot=VariablesToPlot-6
        VariablesToPlotPerLoop=6

      }else{

        VariablesToPlotPerLoop=VariablesToPlot

      }

      for (i in 1:VariablesToPlotPerLoop){

        VariableCounter=VariableCounter+1

        LineData=LoessFromData(data[[VariableCounter]][[1]],alpha = alpha,span = data[[VariableCounter]][[2]])

        if(is.na(custom_age)){

        plot(NA,
             xlim=c(min(min(na.omit(data[[VariableCounter]][[1]])), min(na.omit(LineData$LoessConfDown))),
                    max(max(na.omit(data[[VariableCounter]][[1]])),max(na.omit(LineData$LoessConfUp)))),
             ylim=c(max(MaxForAll),min(MinForAll)),
             ylab="Age",
             xlab="value",
             main=paste(names(data[VariableCounter]),data[[VariableCounter]][[2]])

        )

          customlabels=seq(from=0,
                           to =ceiling(max(MaxForAll)/1000)*1000,
                           by =1000)

          axis(2,at=customlabels,labels = character(length(customlabels)))

        }else{

          plot(NA,
               xlim=c(min(min(na.omit(data[[VariableCounter]][[1]])), min(na.omit(LineData$LoessConfDown))),
                      max(max(na.omit(data[[VariableCounter]][[1]])),max(na.omit(LineData$LoessConfUp)))),
               ylim=c(custom_age,min(MinForAll)),
               ylab="Age",
               xlab="value",
               main=paste(names(data[VariableCounter]),data[[VariableCounter]][[2]])
          )

          customlabels=seq(from=0,
                           to =ceiling(custom_age/1000)*1000,
                           by =1000)

          axis(2,at=customlabels,labels = character(length(customlabels)))

        }

        points(na.omit(cbind(data[[VariableCounter]][[1]],as.numeric(names(data[[VariableCounter]][[1]])))),pch = 19,cex=0.5)

        lines(as.numeric(names(LineData$LoessMean)) ~ LineData$LoessMean)
        lines(as.numeric(names(LineData$LoessConfUp)) ~ LineData$LoessConfUp,lty=3)
        lines(as.numeric(names(LineData$LoessConfDown)) ~ LineData$LoessConfDown,lty=3)

      }
    }
  }

  par(mfrow=c(1,1))

}
