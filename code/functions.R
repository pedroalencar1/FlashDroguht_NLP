
# function to get pentad number. Leap year agregated in February
get_pentad <- function(time){
  julian <- lubridate::yday(time)
  leap <- lubridate::leap_year(time)
  year <-  lubridate::year(time)
  
  pentad <- ceiling((julian + leap*((julian >= 60)*-1))/5) 
  pentad <- pentad + (year-1980)*73
  
  return(pentad)
}


#' Ford and Labosier method for raster brick
#'
#' @param swc data frame with dates (as pentads) and sm (mean in the pentad)
#' @param crit 
#'
#' @return
#' @examples
raster_FL2017 <- function(swc, crit = c(40, 20, 30)){
  # crit = c(40, 20, 30)
  # swc = df
  
  crit1 = crit[1]
  crit2 = crit[2]
  crit3 = crit[3]
  
  colnames(swc) <- c("time", "swc")
  
  #get series and matrix of soil moisture
  series.swc <- swc
  pentad.swc <- matrix(swc$swc, nrow = 73, byrow = F)
  
  #get series and matrix of percentiles
  percentile.swc <- t(apply(pentad.swc, 1, fdClassify::f.percentile))
  percentile.series <- c(percentile.swc)
  
  # remove NA, save positions
  firstNonNA <- min(which(!is.na(percentile.series)))
  lastNonNA <- max(which(!is.na(percentile.series)))
  percentile.series <- percentile.series[firstNonNA:lastNonNA]
  
  # get possible intensification intervals
  a1 <- diff(percentile.series, lag = 1) %>% c(rep(100, 1), 
                                               .)
  a2 <- diff(percentile.series, lag = 2) %>% c(rep(100, 2), 
                                               .)
  a3 <- diff(percentile.series, lag = 3) %>% c(rep(100, 3), 
                                               .)
  a4 <- diff(percentile.series, lag = 4) %>% c(rep(100, 4), 
                                               .)
  data.table <- as.data.frame(cbind(percentile.series, a1, 
                                    a2, a3, a4))
  # get highest drop
  data.table$a.min <- sapply(1:nrow(data.table), 
                             function(i) min(data.table[i,2:5], na.rm = T))
  
  # get min intensification period
  data.table$p.min <- apply(data.table[,2:5], 1, which.min)
  
  # get fd onset date
  data.table$fd <- 0
  for (i in 2:(nrow(data.table) - 4)) {
    data.table$fd[i] <- (data.table$percentile.series[i] <= crit2) * 
      (data.table$percentile.series[i - data.table$p.min[i]] >= crit1) * 
      (max(data.table$percentile.series[(i + 1):(i + 3)]) <= crit3)
    
  }
  
  data.table$p.min <- data.table$p.min * data.table$fd
  data.table[is.na(data.table)] <- 0
  
  # get complete duration of fd
  data.table$fd2 <- 0
  for (i in 2:nrow(data.table)){
    if (data.table$fd[i] == 1) {
      # i = 3028
      before <- i-data.table$p.min[i]
      after <- min(i + min(which(data.table$percentile.series[(i+1):nrow(data.table)] > crit1), nrow(data.table)) - 1,
                   nrow(data.table))
      
      data.table$fd2[before:after] <- 1
    }
  }
  
  series.swc$fd <- data.table$fd2
  
  return(series.swc)
}