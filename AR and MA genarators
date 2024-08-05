setClass(Class = "TS", slots = list("param"="numeric","noise_sd" = "numeric",
                                    "series_length" = "numeric", "intercept" = "numeric"))
setClass(Class = "ARS", slots = list("initial_states" = "numeric"), contains = "TS")

setClass(Class = "MAS", contains = "TS")

setGeneric("gen_seq",  function(x) standardGeneric("gen_seq"))

setMethod(
  "gen_seq",
  "ARS",
  function(x) {
    steps <- x@series_length - length(x@initial_states)
    if (steps < 0)
      stop("length (default 100) must be greater than number of initial states")
    if (length(x@initial_states) < length(x@param)) 
      stop("number of initial conditions must equal to or greater than number of parameters") 
    p <- length(x@param) #perhaps use q instead here
    TS <- x@initial_states
    for (k in 1:steps){
      new_occurrence <- sum(tail(TS,p)[p:1] * x@param) + rnorm(1,0,x@noise_sd) + x@intercept #perhaps remove intercept here
      print(new_occurrence)
      TS <- c(TS,new_occurrence)
    }
    return(TS)
  }
)

setMethod("gen_seq",
          "MAS",
        function(x){
        p <- length(x@param)
        errors <- rep(0,p)
        MA_series <- numeric(0)
          for (i in 1:x@series_length){
            error_i <- rnorm(1,mean = 0,sd = x@noise_sd)
            x_i <- sum(tail(errors,p)[p:1] * x@param) + x@intercept + error_i
            errors <-  c(errors,error_i)
            MA_series <- c(MA_series,x_i)
            }
            return(MA_series)
          })
