#' Plot raw data for validation.
#' @param id Patient's data to plot. First if not mentioned
#' @param data Table.
#' @param x.name Name of the X axis.
#' @param y.name Name of the Y axis.
#' @keywords plot raw data
#' @export PlotValid
#' @keywords plot raw
#' @examples # PlotValid(id="1001",data=PK)
#' @import dplyr
#' @import readr
#' @import writexl
#' @import knitr
#' @import latex2exp

PlotValid <- function(id=NULL,data=PK,x.name="Time (day)",y.name="[Cetuximab] (mg/L)"){
  if(is.null(id)){id=data$ID[1]}
  data = data %>% mutate(CENS=as.factor(CENS))%>% group_by(ID) %>%
    mutate(sample="other",
           Tlast = ifelse(is.na(AMT),NA,TIME+AMT/RATE),
           Tnext = ifelse(is.na(AMT),NA,TIME))%>%
    fill(Tlast,.direction = "down") %>%
    fill(Tnext,.direction = "up") %>%
    mutate(Tlast=ifelse(is.na(AMT),TIME -Tlast,NA),
           Tnext=ifelse(is.na(AMT),Tnext- TIME, NA)) %>%
    ungroup() %>%
    mutate(sample=ifelse(Tlast<2/24,"injection",sample),
           sample=ifelse(Tlast>1,"pic",sample),
           sample=ifelse(Tnext<1,"trougth",sample),
           IGNOREOBS = ifelse(sample=="injection",1,0),
           IGNOREOBS = ifelse(is.na(sample),0,IGNOREOBS))
  ggplot(data %>% filter(ID==id&!is.na(Y)))+theme_classic() +
    geom_line(aes(x = TIME, y = Y, group = ID),color="gray", size = .5) +
    geom_point(aes(x = TIME, y = Y,color=sample, shape = CENS)) +
    #scale_y_log10(expand = c(0, 0)) +
    scale_y_continuous(y.name, limits = c(0,100*ceiling(max(data$Y,na.rm = T)/100)), expand = c(0, 0)) +
    scale_x_continuous(x.name, limits = c(0,28*ceiling(max(data$TIME,na.rm = T)/28)), expand = c(0, 0))+
    ggtitle(label = paste("Patient",id) )
}
