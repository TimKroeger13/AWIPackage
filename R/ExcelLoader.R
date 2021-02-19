#'AWIExcelLoader
#'@description Loads an Excel of the AWI database and interpolates it.
#'@export
#'@import vegan stats readxl
#'@param Excelname Excel sheet from the AWI database.
#'@param AgeTxtName Age textfile from Bacon with a d.by from 0.25. This is optional.
#'@param Tables Selecting which table sheets to load.
#'@param Interpolate Bool indicating whether data should be interpolated or not.
#'@return AWIExcelLoader retruns a List of all interpolatet excel sheets.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

AWIExcelLoader = function(Excelname,AgeTxtName=NULL,Tables=c("Organic","Element","GrainSize","Mineral","Diatom"),
                          Interpolate=T){

  minInterpolationValue=0
  minInterpolationName=NA
  maxInterpolationValue=Inf
  maxInterpolationName=NA

  FixExcelRowNames = function(NameRow){

    return(gsub("\r","",gsub("\n","",as.character(NameRow),ignore.case = T),ignore.case = T))

  }

  DisconnectNameAndDepth = function(FirstEntry){

    return(read.table(textConnection(toString(FirstEntry))))

  }

  if (!is.null(AgeTxtName)){

    Age=read.table(AgeTxtName)
    if(!Age[3,1]==0.25){

      warning("Depth intervals at which ages are calculated is not 0.25!
      Please Change is in the Bacon Model!
      Bacon(core=...,thick=...,d.by = 0.25)")
    }
  }

  if(sum(toupper(Tables)=="ORGANIC")>0){

    Organic=suppressMessages(read_excel(path = paste(getwd(),"/",Excelname,sep=""),sheet = 2))
    TempColName=FixExcelRowNames(Organic[6,])
    Organic=Organic[7:dim(Organic)[1],]
    CoreName=toString(DisconnectNameAndDepth(Organic[1,1])[1])
    Organic[,1]=gsub(paste(CoreName," ",sep=""),"",as.matrix(Organic[,1]))
    Organic=suppressWarnings(as.data.frame(matrix(as.numeric(unlist(Organic)),ncol = dim(Organic)[2])))
    TempColName[1]="depth"
    colnames(Organic)=TempColName

    if(Organic[1,1]>minInterpolationValue){

      minInterpolationValue=Organic[1,1]
      minInterpolationName="Organic"
    }
    if(Organic[dim(Organic)[1],1]<maxInterpolationValue){

      maxInterpolationValue=Organic[dim(Organic)[1],1]
      maxInterpolationName="Organic"

    }
  }

  if(sum(toupper(Tables)=="GRAINSIZE")>0){

    GrainSize=suppressMessages(read_excel(path = paste(getwd(),"/",Excelname,sep=""),sheet = 3))
    TempColName=FixExcelRowNames(GrainSize[6,])
    GrainSize=GrainSize[7:dim(GrainSize)[1],]
    CoreName=toString(DisconnectNameAndDepth(GrainSize[1,1])[1])
    GrainSize[,1]=gsub(paste(CoreName," ",sep=""),"",as.matrix(GrainSize[,1]))
    GrainSize=suppressWarnings(as.data.frame(matrix(as.numeric(unlist(GrainSize)),ncol = dim(GrainSize)[2])))
    TempColName[1]="depth"
    colnames(GrainSize)=TempColName

    if(GrainSize[1,1]>minInterpolationValue){

      minInterpolationValue=GrainSize[1,1]
      minInterpolationName="GrainSize"
    }
    if(GrainSize[dim(GrainSize)[1],1]<maxInterpolationValue){

      maxInterpolationValue=GrainSize[dim(GrainSize)[1],1]
      maxInterpolationName="GrainSize"

    }
  }

  if(sum(toupper(Tables)=="ELEMENT")>0){

    Element=suppressMessages(read_excel(path = paste(getwd(),"/",Excelname,sep=""),sheet = 4))
    TempColName=FixExcelRowNames(Element[5,])
    Element=Element[6:dim(Element)[1],]
    CoreName=toString(DisconnectNameAndDepth(Element[1,1])[1])
    Element[,1]=gsub(paste(CoreName," ",sep=""),"",as.matrix(Element[,1]))
    Element=suppressWarnings(as.data.frame(matrix(as.numeric(unlist(Element)),ncol = dim(Element)[2])))
    TempColName[1]="depth"
    colnames(Element)=TempColName

    if(Element[1,1]>minInterpolationValue){

      minInterpolationValue=Element[1,1]
      minInterpolationName="Element"
    }
    if(Element[dim(Element)[1],1]<maxInterpolationValue){

      maxInterpolationValue=Element[dim(Element)[1],1]
      maxInterpolationName="Element"

    }
  }

  if(sum(toupper(Tables)=="MINERAL")>0){

    Mineral=suppressMessages(read_excel(path = paste(getwd(),"/",Excelname,sep=""),sheet = 5))
    TempColName=FixExcelRowNames(Mineral[5,])
    Wavename=FixExcelRowNames(Mineral[6,])
    Wavename=c("","","",Wavename[4:length(Wavename)])
    TempColName=paste(TempColName,Wavename)
    Mineral=Mineral[7:dim(Mineral)[1],]
    CoreName=toString(DisconnectNameAndDepth(Mineral[2,1])[1])
    Mineral[,1]=gsub(paste(CoreName," ",sep=""),"",as.matrix(Mineral[,1]))
    Mineral=suppressWarnings(as.data.frame(matrix(as.numeric(unlist(Mineral)),ncol = dim(Mineral)[2])))
    TempColName[1]="depth"
    colnames(Mineral)=TempColName

    if(Mineral[1,1]>minInterpolationValue){

      minInterpolationValue=Mineral[1,1]
      minInterpolationName="Mineral"
    }
    if(Mineral[dim(Mineral)[1],1]<maxInterpolationValue){

      maxInterpolationValue=Mineral[dim(Mineral)[1],1]
      maxInterpolationName="Mineral"

    }
  }

  if(sum(toupper(Tables)=="DIATOM")>0){

    Diatom=suppressMessages(read_excel(path = paste(getwd(),"/",Excelname,sep=""),sheet = 6))
    TempColName=FixExcelRowNames(Diatom[5,])
    Diatom=Diatom[6:dim(Diatom)[1],]
    CoreName=toString(DisconnectNameAndDepth(Diatom[1,1])[1])
    Diatom[,1]=gsub(paste(CoreName," ",sep=""),"",as.matrix(Diatom[,1]))
    Diatom=suppressWarnings(as.data.frame(matrix(as.numeric(unlist(Diatom)),ncol = dim(Diatom)[2])))
    TempColName[1]="depth"
    colnames(Diatom)=TempColName

    if(Diatom[1,1]>minInterpolationValue){

      minInterpolationValue=Diatom[1,1]
      minInterpolationName="Diatom"
    }
    if(Diatom[dim(Diatom)[1],1]<maxInterpolationValue){

      maxInterpolationValue=Diatom[dim(Diatom)[1],1]
      maxInterpolationName="Diatom"

    }
  }

  if(Interpolate){

    suggestion=T
    stop=F
    while(suggestion){

      cat("\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n",
          "\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n")

      cat(sep="","Boarders for the interpolation:",
          "\n",
          "\n",
          "lower interpolation limit set by ",minInterpolationName,
          "\n",
          "Value = ",minInterpolationValue,
          "\n",
          "\n",
          "Upper interpolation limit set by ",maxInterpolationName,
          "\n",
          "Value = ",maxInterpolationValue,
          "\n",
          "\n",
          "c <- Confrim (Continue script)\n",
          "s <- stop\n\n"
      )

      x <- readline(prompt = "confirm data: ")

      if(tolower(x)=="c"){

        suggestion=F

      }

      if(tolower(x)=="s"){

        return()

      }
    }

    InterploationSeqence=seq(from = minInterpolationValue, to = maxInterpolationValue, by = 0.25)

    if(sum(toupper(Tables)=="ORGANIC")>0){

      Organic.clone=matrix(NA,nrow = length(InterploationSeqence),ncol = dim(Organic)[2])
      Organic.clone[,1]=InterploationSeqence
      colnames(Organic.clone)=colnames(Organic)

      for(i in 2:dim(Organic)[2]){

        if(!sum(is.na(Organic[,i]))==dim(Organic)[1]){

          InterApprox=approx(Organic[,1], Organic[,i], xout = InterploationSeqence, method = "linear",na.rm=T)
          Organic.clone[,i]=InterApprox$y
        }else{

          Organic.clone[,i]=NA

        }
      }
      Organic=as.data.frame(Organic.clone)
    }


    if(sum(toupper(Tables)=="GRAINSIZE")>0){

      GrainSize.clone=matrix(NA,nrow = length(InterploationSeqence),ncol = dim(GrainSize)[2])
      GrainSize.clone[,1]=InterploationSeqence
      colnames(GrainSize.clone)=colnames(GrainSize)

      for(i in 2:dim(GrainSize)[2]){

        if(!sum(is.na(GrainSize[,i]))==dim(GrainSize)[1]){

          InterApprox=approx(GrainSize[,1], GrainSize[,i], xout = InterploationSeqence, method = "linear",na.rm=T)
          GrainSize.clone[,i]=InterApprox$y
        }else{

          GrainSize.clone[,i]=NA

        }
      }
      GrainSize=as.data.frame(GrainSize.clone)
    }

    if(sum(toupper(Tables)=="ELEMENT")>0){

      Element.clone=matrix(NA,nrow = length(InterploationSeqence),ncol = dim(Element)[2])
      Element.clone[,1]=InterploationSeqence
      colnames(Element.clone)=colnames(Element)

      for(i in 2:dim(Element)[2]){

        if(!sum(is.na(Element[,i]))==dim(Element)[1]){

          InterApprox=approx(Element[,1], Element[,i], xout = InterploationSeqence, method = "linear",na.rm=T)
          Element.clone[,i]=InterApprox$y
        }else{

          Element.clone[,i]=NA

        }
      }
      Element=as.data.frame(Element.clone)
    }

    if(sum(toupper(Tables)=="MINERAL")>0){

      Mineral.clone=matrix(NA,nrow = length(InterploationSeqence),ncol = dim(Mineral)[2])
      Mineral.clone[,1]=InterploationSeqence
      colnames(Mineral.clone)=colnames(Mineral)

      for(i in 2:dim(Mineral)[2]){

        if(!sum(is.na(Mineral[,i]))==dim(Mineral)[1]){

          InterApprox=approx(Mineral[,1], Mineral[,i], xout = InterploationSeqence, method = "linear",na.rm=T)
          Mineral.clone[,i]=InterApprox$y
        }else{

          Mineral.clone[,i]=NA

        }
      }
      Mineral=as.data.frame(Mineral.clone)
    }

    if(sum(toupper(Tables)=="DIATOM")>0){

      Diatom.clone=matrix(NA,nrow = length(InterploationSeqence),ncol = dim(Diatom)[2])
      Diatom.clone[,1]=InterploationSeqence
      colnames(Diatom.clone)=colnames(Diatom)

      for(i in 2:dim(Diatom)[2]){

        if(!sum(is.na(Diatom[,i]))==dim(Diatom)[1]){

          InterApprox=approx(Diatom[,1], Diatom[,i], xout = InterploationSeqence, method = "linear",na.rm=T)
          Diatom.clone[,i]=InterApprox$y
        }else{

          Diatom.clone[,i]=NA

        }
      }
      Diatom=as.data.frame(Diatom.clone)
    }
  }

  out=list()

  out$Diatom=Diatom
  out$Element=Element
  out$GrainSize=GrainSize
  out$Mineral=Mineral
  out$Organic=Organic

  return(out)

}
