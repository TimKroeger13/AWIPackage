#'AWIExcelLoader
#'@description Loads an Excel of the AWI database and interpolates it.
#'@export
#'@import compositions vegan readxl
#'@importFrom utils read.csv read.table
#'@param Excelname Excel sheet from the AWI database.
#'@param AgeTxtName Age textfile from Bacon with a d.by from 0.25. This is optional.
#'@param Tables Selecting which table sheets to load.
#'@param FixFormat standardizes all data so that it is easier to calculate with them in the future.
#'@return AWIExcelLoader retruns a List of all interpolatet excel sheets.
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

AWIExcelLoader = function(Excelname,AgeTxtName=NULL,Tables=c("Organic","Element","GrainSize","Mineral","Diatom"),FixFormat=F){

  FixExcelRowNames = function(NameRow){

    return(gsub("\r","",gsub("\n","",as.character(NameRow),ignore.case = T),ignore.case = T))

  }

  DisconnectNameAndDepth = function(FirstEntry){

    return(read.table(textConnection(toString(FirstEntry))))

  }

  DeleteNaRows = function(DataFrame){

    i=0

    while (i<dim(DataFrame)[2]) {

      i=i+1

      if(sum(is.na(DataFrame[,i]))==dim(DataFrame)[1]){

        DataFrame=DataFrame[,-i]

        i=i-1

      }
    }

    return(DataFrame)

  }

  AddAgges = function(depth){

    Ageresult=array(data = NA, dim = length(depth))
    Age=read.table(AgeTxtName)

    for (i in 1:length(depth)){

      Ageresult[i]=as.numeric(Age[match(depth[i],Age[,1]),5])

    }

    return(Ageresult)

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

  }

  if(FixFormat){

    if(sum(toupper(Tables)=="DIATOM")>0){

      Diatom=DeleteNaRows(Diatom)
      Diatom[,3:dim(Diatom)[2]]=clr(Diatom[,3:dim(Diatom)[2]])

    }

    if(sum(toupper(Tables)=="GRAINSIZE")>0){GrainSize=DeleteNaRows(GrainSize)}
    if(sum(toupper(Tables)=="MINERAL")>0){Mineral=DeleteNaRows(Mineral)}
    if(sum(toupper(Tables)=="ORGANIC")>0){Organic=DeleteNaRows(Organic)}

    if(sum(toupper(Tables)=="ELEMENT")>0){

      Element=DeleteNaRows(Element)

      i=1

      while (i<dim(Element)[2]) {

        i=i+1

        if(!grepl("Area",colnames(Element)[i], fixed = TRUE)){

          Element=Element[,-i]

          i=i-1

        }
      }

      Element[,2:dim(Element)[2]]=clr(Element[,2:dim(Element)[2]])

    }
  }

  if(!is.null(AgeTxtName)){

    if(sum(toupper(Tables)=="DIATOM")>0){Diatom[,1]=AddAgges(Diatom[,1])}
    if(sum(toupper(Tables)=="ELEMENT")>0){Element[,1]=AddAgges(Element[,1])}
    if(sum(toupper(Tables)=="GRAINSIZE")>0){GrainSize[,1]=AddAgges(GrainSize[,1])}
    if(sum(toupper(Tables)=="MINERAL")>0){Mineral[,1]=AddAgges(Mineral[,1])}
    if(sum(toupper(Tables)=="ORGANIC")>0){Organic[,1]=AddAgges(Organic[,1])}

  }

  out=list()

  if(sum(toupper(Tables)=="DIATOM")>0){out$Diatom=Diatom}
  if(sum(toupper(Tables)=="ELEMENT")>0){out$Element=Element}
  if(sum(toupper(Tables)=="GRAINSIZE")>0){out$GrainSize=GrainSize}
  if(sum(toupper(Tables)=="MINERAL")>0){out$Mineral=Mineral}
  if(sum(toupper(Tables)=="ORGANIC")>0){out$Organic=Organic}

  return(out)

}
