#'LoessFromData
#'@description Creates a List from the new calibrated vectors of the Loess function with confidence intervals.
#'@export
#'@importFrom stats loess predict qt
#'@param data data from the Folder structure or the whole Folder structur at once.
#'@param alpha size of the confidence intervals.
#'@param span the parameter α which controls the degree of smoothing.
#'@param type decides whether to read the whole fodler or only a sublist.\cr type="vector" for a list \cr type=="folder" for the whole folder
#'@return AWIExcelLoader retruns a List of all interpolatet excel sheets.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

LoessFromData = function(data, alpha = 0.05, span = 0.2, type="vector"){

  OutFolder=list()

  if(type=="folder"){

    for (i in 1:length(ls(data))){

      for (k in 1:length(ls(data[[i]]))){

        x=data[[i]][[k]]
        y=as.numeric(names(data[[i]][[k]]))

        loessValues=predict(loess(x ~ y, span=span), se=T, newdata = as.numeric(y))

        LoessMean = loessValues$fit
        LoessConfUp = loessValues$fit + qt(1-(alpha/2),loessValues$df)*loessValues$se
        LoessConfDown = loessValues$fit - qt(1-(alpha/2),loessValues$df)*loessValues$se

        names(LoessMean)=y
        names(LoessConfUp)=y
        names(LoessConfDown)=y

        OutFolder[[names(data)[i]]][[names(data[[i]])[k]]][["data"]]=data[[i]][[k]]
        OutFolder[[names(data)[i]]][[names(data[[i]])[k]]][["LoessMean"]]=LoessMean
        OutFolder[[names(data)[i]]][[names(data[[i]])[k]]][["LoessConfUp"]]=LoessConfUp
        OutFolder[[names(data)[i]]][[names(data[[i]])[k]]][["LoessConfDown"]]=LoessConfDown

      }
    }
  }

  if(type=="vector"){

    x=data
    y=names(data)

    loessValues=predict(loess(x ~ y, span=span), se=T, newdata = as.numeric(y))

    LoessMean = loessValues$fit
    LoessConfUp = loessValues$fit + qt(1-(alpha/2),loessValues$df)*loessValues$se
    LoessConfDown = loessValues$fit - qt(1-(alpha/2),loessValues$df)*loessValues$se

    names(LoessMean)=y
    names(LoessConfUp)=y
    names(LoessConfDown)=y

    OutFolder[["data"]]=data
    OutFolder[["LoessMean"]]=LoessMean
    OutFolder[["LoessConfUp"]]=LoessConfUp
    OutFolder[["LoessConfDown"]]=LoessConfDown

  }

  return(OutFolder)

}

