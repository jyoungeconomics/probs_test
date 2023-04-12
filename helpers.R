# https://towardsdatascience.com/implied-volatility-in-r-assess-options-risk-correctly-70fe36843474
gBSM <- function(S, X, sigma, r, q, ttm, type){
  #S = stock price
  #X = strike price
  #sigma = volatility
  #r = risk free interest rate
  #q = dividend yield
  #ttm = time to maturity in days
  #type = option type
  
  b <- r - q
  t <- ttm/365.25
  
  d1 <- (log(S / X) + (b + sigma ^ 2 / 2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  
  if(type == "call"){
    price <- S * exp((b - r) * t) * pnorm(d1) - X * exp(-r * t) * pnorm(d2)
  }else if (type == "put"){
    price <-  (X * exp(-r * t) * pnorm(-d2) - S * exp((b - r) * t) * pnorm(-d1))
  }
  
  return(price)
}

volOptimFun <- function(sigma, price, S, K, r, q, ttm, type){
  crossprod(price - gBSM(S, K, sigma, r, q, ttm, type))
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

goobus <- function(z,details=F){
  z <- ifelse(z>100,z/100,z)
  #Pr[x<QQQ] from lognormal dist
  temp <- plnorm(q = z,meanlog = DDD$MOO,sdlog = DDD$SIG)
  if(details==F){
    return(cbind.data.frame("date"=DDD$Date[length(temp)],
                            "ppp"=temp[length(temp)]))
  }else(return(cbind.data.frame("date"=DDD$Date,
                                "ppp"=temp)))
}
