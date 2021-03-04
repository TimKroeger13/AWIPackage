#'CodeforBoris
#'@description Code Sampels for Boris.
#'@export
#'@return Return perfect code.
#'@author Tim Kröger
#'@note This function has only been developed for Boris Biskaborn.

CodeforBoris = function(){

  cat(sep="","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n",
      "\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n",'


data=AWIExcelLoader(Excelname = "PG2208_raw_data.xlsx",AgeTxtName = "PG2208_77_ages.txt",FixFormat = T)

Diatom=data$Diatom
Element=data$Element
GrainSize=data$GrainSize
Mineral=data$Mineral
Organic=data$Organic




Sigdata=read.csv("Sigements.csv")

Faktor1=InterpolateVector(age = Sigdata[,7],data = Sigdata[,8],thick = 50)
Faktor2=InterpolateVector(age = Mineral[,1],data = Mineral[,3],thick = 50)

VectorCorrelation(x = Faktor1,y = Faktor2,offset = 0)


PlotData=VectorCorrelationAll(x=c(Sigdata[,7],Sigdata[,8]),
                     y=c(Mineral[,1],Mineral[,3]),
                     thick = 50,
                     offset = 2000)



plot(PlotData$main,xaxt="n",type = "p")
axis(1,seq(1,81,10),names(PlotData$main)[seq(1,81,10)])



  '
  )
}
