# Univarent

dice <- function(){
     roll <- runif(n = 1, min = 1, max = 7)
     roll <- as.integer(roll)
     return(roll)
}
