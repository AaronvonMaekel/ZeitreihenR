setClass("TimeSeries", 
    slots = c()
)
setClass("AR", 
    slots = c(
        AR_cof = "numeric",
    ),
    contains="TimeSeries",
    prototype = list(AR_cof = NA_real_)
)
setClass("MA",
    slots = c(
        MA_cof = "numeric",
        ),
    contains="TimeSeries",
    prototype = list(AR_cof = NA_real_)
)