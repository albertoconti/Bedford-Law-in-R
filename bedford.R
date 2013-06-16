dbenford <- function(x){
    log10(1 + 1/x)
}
 
pbenford <- function(q){
    cumprobs <- cumsum(dbenford(1:9))
    return(cumprobs[q])
}
 
qbenford <- function(p){
    cumprobs <- cumsum(dbenford(1:9))
    cumprobs[9] <- 1 # To fix a rounding error
    quantiles <- sapply(p, function(x) {10 - sum(cumprobs >= x)})
    return(quantiles)
}
 
rbenford <- function(n){
    sample(1:9, size = n, replace = TRUE, prob = dbenford(1:9))
}

BenfordObsExp <- function(x){
    data <- substitute(x)
    n <- length(x)
    # Peel off the first digit
    x <- as.numeric(substring(formatC(x, format = 'e'), 1, 1))
    obsFreq <- tabulate(x, nbins = 9)
    benFreq <- n * dbenford(1:9)
    plot(1:9, benFreq, xlim = c(1, 9), ylim = c(0, max(obsFreq, benFreq)), type = 'l',
        main = paste(data, ": observed (red) and expected (Benford's Law)"),
        xlab = "Digit", ylab = "Frequency")
    axis(1, at = 1:9)
    points(1:9, obsFreq, col = "red", pch = 16)
}

par(mfrow=c(2, 3))
BenfordObsExp(AirPassengers)
BenfordObsExp(EuStockMarkets)
BenfordObsExp(eurodist)
BenfordObsExp(islands)
BenfordObsExp(lynx)
