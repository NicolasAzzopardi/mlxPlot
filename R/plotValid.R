#' Plot raw data for validation.
#' @param id Patient's data to plot. First if not mentioned
#' @param data PK table.
#' @param x.name Name of the X axis.
#' @param y.name Name of the Y axis. e.g. "[Drug] (mg/L)"
#' @keywords plot raw data
#' @export PlotValid
#' @keywords plot raw
#' @examples # PlotValid(id="1001",data=PK)
#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom readr read_delim
#' @import writexl
#' @import knitr
#' @import latex2exp
#' @importFrom rlang .data

PlotValid <- function(id=NULL,data=NULL,x.name="Time (day)",y.name=NULL){
  if(is.null(id)){id=data$ID[1]}
  data = data %>% mutate(CENS=as.factor(.data$CENS))%>% group_by(.data$ID) %>%
    mutate(sample="other",
           Tlast = ifelse(is.na(.data$AMT),NA,.data$TIME+.data$AMT/.data$RATE),
           Tnext = ifelse(is.na(.data$AMT),NA,.data$TIME))%>%
    fill(.data$Tlast,.direction = "down") %>%
    fill(.data$Tnext,.direction = "up") %>%
    mutate(Tlast=ifelse(is.na(.data$AMT),.data$TIME -.data$Tlast,NA),
           Tnext=ifelse(is.na(.data$AMT),.data$Tnext- .data$TIME, NA)) %>%
    ungroup() %>%
    mutate(sample=ifelse(.data$Tlast<2/24,"injection",.data$sample),
           sample=ifelse(.data$Tlast>1,"pic",.data$sample),
           sample=ifelse(.data$Tnext<1,"trougth",.data$sample),
           IGNOREOBS = ifelse(.data$sample=="injection",1,0),
           IGNOREOBS = ifelse(is.na(.data$sample),0,.data$IGNOREOBS))
  ggplot(data %>% filter(.data$ID==id&!is.na(.data$Y)))+theme_classic() +
    geom_line(aes(x = .data$TIME, y = .data$Y, group = .data$ID),color="gray", size = .5) +
    geom_point(aes(x = .data$TIME, y = .data$Y,color=.data$sample, shape = .data$CENS)) +
    #scale_y_log10(expand = c(0, 0)) +
    scale_y_continuous(y.name, limits = c(0,100*ceiling(max(data$Y,na.rm = T)/100)), expand = c(0, 0)) +
    scale_x_continuous(x.name, limits = c(0,28*ceiling(max(data$TIME,na.rm = T)/28)), expand = c(0, 0))+
    ggtitle(label = paste("Patient",id) )
}
