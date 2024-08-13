# Durbin-Levinson Algorithm function

dla <- function(ts_obj){
    # Save number of values in timeseries
    max_n <- ts_obj@n
    
    # Computing the ACF values
    acf_compl <- c( sapply(1:(max_n-1), \(x) {ACF(ts_obj, x)}), 0)
    
    # Compute coefficients
    for (n in 1:max_n) {
        if (n == 1) { 
            # Initialization
            phi <- ACF(ts_obj, 1) / ACF(ts_obj, 0)
            phi_n <- phi
            nu <- ACF(ts_obj, 0) * (1 - phi^2)
        } else {
            # Recursive computation
            val <- sapply(1:(n-1), \(j) {ACF(ts_obj, n-j) * phi[j]})
            
            # Update values of phi and nu
            phi_n <- (1/nu)*(acf_compl[n] - sum(val))
            phi <- phi - phi_n * rev(phi)
            phi <- c(phi, phi_n)
            nu <- nu*(1 - phi_n*phi_n)
        }
    }
    return(phi) 
}


# Predictor based on the DL algorithm
dl_predictor <- function(ts_obj, pred_len=1, entire_ts = TRUE){
    
    # Checking validity
    stopifnot("Prediction length should be greater or equal to 1" =  pred_len >= 1)
    stopifnot("Entered preditcion length not compatible" = length(pred_len) == 1)
    stopifnot("Prediction length is not an integer" = pred_len %% 1 == 0)
    
    stopifnot("entire_ts has to be logical" = is.logical(entire_ts))
    stopifnot("Entered value for entire_ts is not compatible" = length(entire_ts) == 1)
    
    in_len <- ts_obj@n
    for (j in 1:pred_len){
        # Computing estimate using coefficients obtained from the DL algorithm
        new_val <- sum(dla(ts_obj) * rev(ts_obj@data))
        
        # Appending estimated value to the time series (needed for further estimates)
        ts_obj@data <- c(ts_obj@data, new_val)
        ts_obj@n <- ts_obj@n + 1
    }
    vec <- ts_obj@data

    # Producing output according to the specification made by entire_ts
    if (entire_ts==TRUE){
        return(vec_to_ts(vec))
    } else {
        vec <- vec[(in_len + 1):(in_len + pred_len)]
        return(vec_to_ts(vec)) 
    }
}

# Testing
dl_predictor(ar_time_series, pred_len = 5, entire_ts = FALSE)
