#----------------------------
# auto covariance function
setGeneric("ACF", 
           function(ts_obj,h) standardGeneric("ACF"))
setMethod("ACF",
          "TimeSeries", function(ts_obj,h){
              #validierungen
              validObject(ts_obj)
              stopifnot("Index is not atomic"=is.atomic(h)) 
              stopifnot("Index is not of type numeric"=is.numeric(h))
              stopifnot("Index is not a integer"=(h %% 1 == 0))
              stopifnot("index out of bounds"=abs(h)<ts_obj@n)
              
              ts <- ts_obj@data
              n <- ts_obj@n
              smpl_mean <- mean(ts)
              
              summe <-  (1/n) * (ts[(1+abs(h)):n]-smpl_mean) %*% (ts[1:(n-abs(h))]-smpl_mean)
              return(summe[1][1])
          })


plot(sapply(1:(ma_time_series@n-1),function(h){ACF(ma_time_series,h)}))

