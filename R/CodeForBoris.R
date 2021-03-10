#'CodeforBoris
#'@description Code Sampels for Boris.
#'@export
#'@return Return perfect code.
#'@author Tim Kröger
#'@note This function has only been developed for Boris Biskaborn.

CodeforBoris = function(){

  cat(sep="","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n",
      "\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n","\n",'

setwd("M:/AWI 2021/Triple B Project")

data=AWIExcelLoader(Excelname = "PG2208_raw_data.xlsx",AgeTxtName = "PG2208_77_ages.txt",FixFormat = T,
                    Tables = c("Organic", "Element", "GrainSize", "Mineral"))

Sigdata=read.csv("Sigements.csv")









Folder=FolderCreater(data = data,format = "collective")
Folder$other=FolderCreater(data = Sigdata,format = "segment")












OrganicList=list()
OrganicList$NI=Folder$Organic$`Nitrogen (TN, %)`
OrganicList$TC=Folder$Organic$`Total Carbon (TC, %)`
OrganicList$TOC=Folder$Organic$`Total Organic Carbon (TOC, %)`

GrainSizeList=list()
GrainSizeList$FinSilt=Folder$GrainSize$`Fine Silt (%)`
GrainSizeList$MediumSilt=Folder$GrainSize$`Medium Silt (%)`
GrainSizeList$CoarseSilt=Folder$GrainSize$`Coarse Silt (%)`

MineralList=list()
MineralList$Qaurtz=Folder$Mineral$`Quartz 3.34`
MineralList$Plagioclase=Folder$Mineral$`Plagioclase 3.19`
MineralList$Hornblende=Folder$Mineral$`Hornblende 8.4`




OrganicList=ListCorrelation(main = Folder$other$MDS_Dim1_Site,dataAsList = OrganicList,thick = 10,offset = 4000)
GrainSizeList=ListCorrelation(main = Folder$other$MDS_Dim1_Site,dataAsList = GrainSizeList,thick = 10,offset = 4000)
MineralList=ListCorrelation(main = Folder$other$MDS_Dim1_Site,dataAsList = MineralList,thick = 10,offset = 4000)

OrganicList=AddIntervalsToFolder(ListCorrelation = OrganicList,intervall = 0.05)
GrainSizeList=AddIntervalsToFolder(ListCorrelation = GrainSizeList,intervall = 0.05)
MineralList=AddIntervalsToFolder(ListCorrelation = MineralList,intervall = 0.05)






main=list()
main$OrganicList=OrganicList
main$GrainSizeList=GrainSizeList
main$MineralList=MineralList





PlotDeviation(main,thick = 50,offset = 4000,plotName="Merge2",colors=c("black","red","blue"))




  '
  )
}
