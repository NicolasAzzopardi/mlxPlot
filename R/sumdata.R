#' Create summary - n, mean, sd, min, max of all numerical data
#'
#' @param dt a data.table with at least one numeric column
#' @return A data.table with summary stats: n, min, max, mean, sd
#' @import data.table
#' @export
#'


sumData <- function(dt) {

  dtNames <- names(dt)
  n <- length(dtNames)

  varname <- list()
  N <- list()
  missing <- list()
  means <- list()
  sds <- list()
  maxs <- list()
  mins <- list()
  median <- list()

  table.t <- list()

  j <- 0
  k <- 0

  for (i in (1:n)) {

    colI <- dt[,get(dtNames[i])]

    if (is.numeric(colI)) {

      j <- j + 1

      varname[[j]] <- dtNames[i]
      N[[j]] <- sum(!is.na(colI))
      missing[[j]] <- sum(is.na(colI))
      maxs[[j]] <- max(colI, na.rm = TRUE)
      mins[[j]] <- min(colI, na.rm = TRUE)
      means[[j]] <- mean(colI, na.rm = TRUE)
      sds[[j]] <- sd(colI, na.rm = TRUE)
      median[[j]] <- median(colI, na.rm = TRUE)

    } else if (is.factor(colI)) {

      k <- k + 1
      ct <- data.table(var=colI)
      tt <- ct[,.N,,keyby=var][, .(var, count=N, pct = round(N/sum(N),3))]
      setnames(tt,"var", dtNames[i])

      table.t[[k]] <- tt

    }
  }

  sumdat <- data.table::data.table(varName = unlist(varname),
                                   N = unlist(N),
                                   missing = unlist(missing),
                                   min = unlist(mins),
                                   max = unlist(maxs),
                                   mean = unlist(means),
                                   sd = unlist(sds),
                                   median = unlist(median)
  )

  return(list(sumdat, table.t))

}
