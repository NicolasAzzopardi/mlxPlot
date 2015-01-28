#' A function to plot raw data in pdf before modelling
#'
#' This function allows you to have a first visualisation of data file.
#' @param TAB Data Table.
#' @param ADM Which ADM to plot.
#' @param FileName Output pdf file name.
#' @param ADM2 Second ADM to plot on right axis (optional).
#' @keywords monolix
#' @export
#' @examples
#' Valid.Plot()

Valid.Plot <- function(TAB,ADM,FileName,ADM2 = NA){
today = gsub("-","",substr(Sys.Date(),3,10))

## Couleur des points
TAB$cola = "orange"
TAB2 = TAB[FALSE,]
IDs = unique(TAB$ID)
for( id in IDs){
	TABid = TAB[TAB$ID==id,]
	for(li in 1:nrow(TABid)){
		if(TABid$YTYPE[li]==ADM){
		TDP = TABid$TIME[TABid$ADM==ADM]
		TFP = TABid$TIME[TABid$ADM==ADM] + as.numeric(TABid$AMT[TABid$ADM==ADM])/as.numeric(TABid$RATE[TABid$ADM==ADM])
		## résiduelle
		if(any(TDP>=TABid$TIME[li]&TDP<(TABid$TIME[li] + 6/24))){
			TABid$cola[li] = "green"
		}
		# pic
		if(any(TDP<TABid$TIME[li]&TFP>(TABid$TIME[li] - 4/24))){
			TABid$cola[li] = "red"
		}
		# pdt perf
		if(any(TDP<TABid$TIME[li]&TFP>TABid$TIME[li])){
			TABid$cola[li] = "black"
		}
	}
}
	TAB2 = rbind(TAB2,TABid)
}
cola = TAB2$cola ; TAB = TAB2[,-grep("cola",names(TAB2))] ; rm(TABid,TAB2)

#limites des plots
xLIM = c(0,ceiling(max(TAB$TIME)/7)*7)
yLIM = c(0,ceiling(max(TAB$DV[TAB$YTYPE==ADM])/10)*10)
# yLIM = c(0,150)

pdf(file = paste(FileName,"_Validation",".pdf",sep=""),width=29.7/cm(1), height=21/cm(1), onefile=TRUE, bg="transparent", pointsize=12)
for(id in unique(TAB$ID)){
cola_id = cola[TAB$ID==id&TAB$YTYPE==ADM]
with(TAB[TAB$ID==id&TAB$YTYPE==ADM,]
	,plot(TIME ,DV ,col=cola_id ,pch=19 ,type="p"
		,xlim= xLIM,ylim=yLIM
		,main=id ,xaxt="n" ,yaxt="n" ,frame=FALSE ,las=1
	)
)
with(TAB[TAB$ID==id&TAB$YTYPE==ADM,] ,lines(TIME ,DV ,col="gray"))

## barre injection
TABidADM = TAB[TAB$ID==id&TAB$ADM==ADM,]
T1 = TABidADM$TIME
T2 = T1
if(any(names(TAB)=="RATE")){ T2 = with(TABidADM, TIME+AMT/RATE) }
xx = c(rep(c(t(matrix(c(T1,T2),ncol=2))),each=2) , T1[1])
yy = c( rep(c(yLIM,rev(yLIM)),nrow(TABidADM)) , yLIM[1])
polygon(xx,yy,border = "#00000000",col=c("#0000FA20","#00FA0020"))
# rgb(0,0,.98,RATE/max(RATE))

with( TAB[TAB$ID==id&TAB$ADM==ADM,], text(T2,yLIM[2]*.95,AMT,pos=4,offset=.5,srt=90,adj=c(1,0) ,col="#0000FF60"))

axis(2,seq(from=0,to=yLIM[2],by=10),pos=0,las=1 ) 
axis(2,seq(from=0,to=yLIM[2],by=5),label=FALSE,pos=0 ) 

if(!is.na(ADM2)){
yLIM2 = c(0,ceiling(max(TAB$DV[TAB$YTYPE==ADM2])/10)*10)
par(new=TRUE)
with( TAB[TAB$ID==id&TAB$YTYPE==ADM2,] 
	,plot(TIME ,DV ,col="skyblue" ,pch=15 ,type="p"
	,xlim=xLIM ,ylim=yLIM2 ,main="" ,xaxt="n" ,yaxt="n" ,frame=FALSE
	)
)
with(TAB[TAB$ID==id&TAB$YTYPE==ADM2,] ,lines(TIME ,DV ,col="gray"))
axis(4 ,seq(from=0 ,to=yLIM2[2] ,by=10) ,pos=xLIM[2],las=1 ) 
axis(4 ,seq(from=0 ,to=yLIM2[2] ,by=5 ) ,label=FALSE,pos=xLIM[2]) 

}

## Ecrit la dose
with( TAB[TAB$ID==id&TAB$ADM==ADM,], axis(1,seq(from=0,to=ceiling(max(TIME)/7)*7,by=7),pos=yLIM[1] ) ) 
with( TAB[TAB$ID==id&TAB$ADM==ADM,], axis(1,seq(from=0,to=ceiling(max(TIME)/7)*7,by=1),label=FALSE,pos=yLIM[1] ) ) 
}
dev.off()
}
