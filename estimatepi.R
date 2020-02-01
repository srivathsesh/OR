#' This function estimates the value of pi using a unit circle.
#'
#' @param N, integer value denoting the number of random points to generate
#'
#' @return data frame with estimate of pi, standard error of estimate and the 95% confidence interval
#' @export
#'
#' @examples estimatepi(1000)
estimatepi <- function(N){
  library(magrittr)
  library(dplyr)
  
# Function to flag points that are inside unit circle.
  insidecircle <- function(x,y){
    ifelse(x^2+y^2 <= 1,1,0)
  }
  
  df <- data.frame(x = abs(runif(N)),
                   y = abs(runif(N)))
  df %<>% dplyr::mutate(selected = insidecircle(x,y))
  
  # Get estimate of points that fall with a unit quarter-circle
  pi_estimate <- df %>% 
    summarise(estimate = sum(selected)/n()*4) %>% 
    as.numeric(.)
  # Store the proporation for ease of computation below
  prop <- sum(df$selected)/nrow(df)
  #CI <- prop.test(sum(df$selected),nrow(df))$conf.int
  
 StdErrorProp <- sqrt(prop*(1-prop)/nrow(df))
 E <- qnorm(.975) * StdErrorProp
 
 CI <- c(prop- E,prop + E)
 
 pi_se <- StdErrorProp * 4
 pi_CI <- CI * 4
 
 # Return estimate of pi, the standard error and the 95% confidence interval
 
 data.frame(pi_estimate = pi_estimate,
            pi_StdError = pi_se,
            LowerConfLimit = pi_CI[1],
            UpperConfLimit = pi_CI[2])
 
}