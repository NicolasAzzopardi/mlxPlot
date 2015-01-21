rm(list = ls())
setwd("C:/Users/Nicolas/Documents/Etudes/AVATAXHER/Data/Modifié")
library(RODBC)
library(lubridate)
source("C:/Users/Nicolas/Documents/Misc/R/Fonctions/Validation_Plot3.R")



today = gsub("-","",substr(Sys.Date(),3,10))

## Tableau de données dosages TTZ
xlsConnect <- odbcConnectExcel("./140410_Data_NA.xls")
DAT = sqlFetch(xlsConnect, "RECUP",as.is=TRUE)#[,c(2:12 , 17)]
ETAT = sqlFetch(xlsConnect, "ETAT",as.is=TRUE)#[,c(2:12 , 17)]
SUB = sqlFetch(xlsConnect, "subject",as.is=TRUE)#[,c(2:12 , 17)]
odbcClose(xlsConnect)



## Tri fct Temps et ID
DAT = DAT[order(DAT$TIME),]
DAT = DAT[order(DAT$ID),]


TAB =with(DAT,data.frame(ID=ID,TIME=TIME,ADM = as.numeric(prel=="P"),YTYPE = as.numeric(prel!="P"),AMT=dosetot,DV=Y))
TAB = TAB[!is.na(TAB$TIME),]
TAB$RATE=4000

TAB$AMT[is.na(TAB$AMT)&TAB$ADM==1] = 0
TAB$DV[is.na(TAB$DV)&TAB$YTYPE==1] = 0

FileName = paste(today,"_ValidTTZ",sep="")
PLOT_Valid(TAB,1,FileName)












DATlite = DAT[,1:10]
DATlite = merge(DATlite,ETAT)
DATlite[DATlite$ETAT=="OK",]

## Ecriture du fichier
write.table(prettyNum(DAT),paste("../../Monolix/01_Databases/",today,"_brut.txt",sep=""), quote=FALSE, append=FALSE, sep = " ", col.names =TRUE, row.names = FALSE)
write.table(prettyNum(DATlite),paste("../../Monolix/01_Databases/",today,"_brutOK.txt",sep=""), quote=FALSE, append=FALSE, sep = " ", col.names =TRUE, row.names = FALSE)



write.table(prettyNum(unique(DAT$ID)),paste("../../Monolix/01_Databases/",today,"_IDs.txt",sep=""), quote=FALSE, append=FALSE, sep = " ", col.names =TRUE, row.names = FALSE)



