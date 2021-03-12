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


testdata=read.csv("test.csv")




Folder=FolderCreater(data = data,format = "collective")
Folder$other=FolderCreater(data = Sigdata,format = "segment")

Folder$test=FolderCreater(data = testdata,format = "segment")






Ogranic=list()
Ogranic$NI=Folder$Organic$`Nitrogen (TN, %)`
Ogranic$TC=Folder$Organic$`Total Carbon (TC, %)`
Ogranic$TOC=Folder$Organic$`Total Organic Carbon (TOC, %)`

Ogranic=ListCorrelation(main = Folder$other$MDS_Dim1_Site,dataAsList = Ogranic,thick = 10,offset = 1000)
Ogranic=AddIntervalsToFolder(ListCorrelation = Ogranic,intervall = 0.05)

main=list()
main$Ogranic=Ogranic

PlotDeviation(data=main,thick = 10,offset = 1000,plotName="H0 + H1 + H2 +Dim1",colors=c("black","red","blue"))












##################



Diatom=list()
Diatom$Hill0=Folder$other$Hill0
Diatom$Hill1=Folder$other$Hill1
Diatom$Hill2=Folder$other$Hill2
Diatom$MDS_Dim1_Site=Folder$other$MDS_Dim1_Site



Diatom=ListCorrelation(main = Folder$other$TJul,dataAsList = Diatom,thick = 10,offset = 1000)
Diatom=AddIntervalsToFolder(ListCorrelation = Diatom,intervall = 0.05)



main=list()
main$Diatom=Diatom

PlotDeviation(data=main,thick = 10,offset = 1000,plotName="H0 + H1 + H2 +Dim1",colors=c("black","red","blue"))








Clustic=list()
Clustic$Ti_Area=Folder$Element$Ti_Area
Clustic$Zr_Area=Folder$Element$Zr_Area
Clustic$K_Area=Folder$Element$K_Area
Clustic$TotalSand=Folder$GrainSize$`Total Sand (%)`
Clustic$Kfeldspart=Folder$Mineral$`K-Feldspar 3.24`
Clustic$Plagioclase=Folder$Mineral$`Plagioclase 3.19`



Clustic=ListCorrelation(main = Folder$other$MDS_Dim1_Site,dataAsList = Clustic,thick = 10,offset = 1000)
Clustic=AddIntervalsToFolder(ListCorrelation = Clustic,intervall = 0.05)

main=list()
main$Clustic=Clustic

PlotDeviation(main,thick = 10,offset = 1000,plotName="Merge2",colors=c("black","red","blue"))




#plots


data=Folder$other$Hill1
PlotFolder(data = Folder$other$Hill1,name = "Hill1")


TestPlot=list()
TestPlot$Value_1=Folder$test$value1
TestPlot$Value_2=Folder$test$value2

PlotFolder(data = TestPlot)


##Output

outData=MaxKorrelation(main = Folder$other$TJul,folder = Folder,thick = 10,offset = 1000)
outData2=MaxKorrelation(main = Folder$other$MDS_Dim1_Site,folder = Folder,thick = 10,offset = 1000)














###test

Test=list()
Test$Value2=Folder$test$value2

Test=ListCorrelation(main = Folder$test$value1,dataAsList = Test,thick = 10,offset = 4000)
Test=AddIntervalsToFolder(ListCorrelation = Test,intervall = 0.05)

main=list()
main$Test=Test

PlotDeviation(data =main,thick = 10,offset = 4000,plotName="Value2",colors=c("black","red","blue"))



Test=list()
Test$Value1=Folder$test$value1

Test=ListCorrelation(main = Folder$test$value2,dataAsList = Test,thick = 10,offset = 4000)
Test=AddIntervalsToFolder(ListCorrelation = Test,intervall = 0.05)

main=list()
main$Test=Test

PlotDeviation(data =main,thick = 10,offset = 4000,plotName="Value1",colors=c("black","red","blue"))






  '
  )
}
