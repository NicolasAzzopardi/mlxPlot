




addCENS.LLOQ <- function(TAB,LLOQ = .1){
  TAB %>%
    mutate(Y = ifelse(is.na(Y),NA,ifelse(Y <=LLOQ,LLOQ,Y)),
           CENS = ifelse(is.na(Y),NA,ifelse(Y==LLOQ, 1, 0)))
}

