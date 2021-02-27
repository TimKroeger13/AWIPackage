#'CodeforBoris
#'@description Code Sampels for Boris.
#'@export
#'@return Return perfect code.
#'@author Tim Kröger
#'@note This function has only been developed for Boris Biskaborn.

CodeforBoris = function(){

  cat(sep="","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n",
      "\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n",'

  Clear
  dev.off()
  rm(list = ls())
  par(mfrow=c(1,1))

  setwd("M:/AWI 2021/Triple B Project")


  data=AWIExcelLoader(Excelname = "PG2208_raw_data.xlsx",Suggest = F,Interpolate = F,Tables = c("Diatom"))

  Diatom=data$Diatom

  data2=AWIExcelLoader(Excelname = "PG2208_raw_data.xlsx",Suggest = F,Interpolate = T,Tables = c("Organic", "Element", "GrainSize", "Mineral"))

  resampledData=Resample(SourceData = Diatom,Datalist = data2)



  a=resampledData$Mineral

  '
  )
}
