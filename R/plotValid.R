#' Plot raw data for validation.
#' @param id Patient's data to plot.
#' @param data Table.
#' @keywords plot raw data
#' @export PlotValid
#' @keywords plot raw
#' @examples # PlotValid(id="1001",data=PK)

PlotValid <- function(id="1001",data=PK){
  ggplot(data %>% filter(ID==id&!is.na(Y)))+
    theme_classic() +
    geom_line(aes(x = TIME, y = Y, group = ID), color = "skyblue", size = .5) +
    geom_point(aes(x = TIME, y = Y)) +
    #scale_y_log10(expand = c(0, 0)) +
    #scale_x_continuous(limits = c(0, 300), expand = c(0, 0)) +
    xlab("Time (day)") + ylab("[Cetuximab] (mg/L)")
}
