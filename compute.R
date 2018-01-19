##' Equivalent Rates under different Compounding Conventions
##' 
##' Converts an interest rate from one compounding convention to
##' another (for example from semi-annual to monthly compounding or
##' from annual to continuous compounding)
equiv.rate <- function(rate, from.freq = 1, to.freq = 1){
  cc.rate <- ifelse(from.freq == Inf, rate,
                    log(1+rate/from.freq)*from.freq)
  if (to.freq == Inf) cc.rate else (exp(cc.rate/to.freq)-1)*to.freq
}

##' Generalized Black Scholes model for pricing vanilla European options
##'
##' Compute values of call and put options as well as the Greeks -
GenBS <- function(s, X, r, Sigma, t, div_yield = 0){
  g = r - div_yield ## the growth rate of the asset price
  d1 <- (log(s / X) + (g + Sigma*Sigma / 2) * t) / (Sigma * sqrt(t))
  ## the division above can be 0/0=NaN if s=X and either t=0 or g=Sigma=0
  ## we set d1 to 0 in this case since numerator is of higher order in t and Sigma
  d1 <- ifelse(is.nan(d1), 0, d1)
  d2 <- d1 - Sigma * sqrt(t)
  Nd1 <- pnorm(d1)
  Nd2 <- pnorm(d2)
  Nminusd1 <- pnorm(-d1)
  Nminusd2 <- pnorm(-d2)
  ## Black Scholes call price
  call <- exp(-r * t) * (s * exp(g * t) * Nd1 - X * Nd2)
  put <- exp(-r * t) * (-s * exp(g * t) * Nminusd1 + X * Nminusd2)
  callDelta <- exp(-div_yield*t)*Nd1
  putDelta <- -exp(-div_yield*t)*Nminusd1
  a <- -s * dnorm(d1) * Sigma * exp(-div_yield*t) / (2 * sqrt(t))
  ## if t is 0, the above is 0/0 = NaN but dnorm(d1) term goes to 0 faster
  ## so we set the result to 0 in this case
  a <- ifelse(is.nan(a), 0,  a)
  b = r * X * exp(-r * t) * Nd2
  c = div_yield * s * Nd1* exp(-div_yield*t)
  callTheta <- a - b + c
  b = r * X * exp(-r * t) * Nminusd2
  c = div_yield * s * Nminusd1 * exp(-div_yield*t)
  putTheta <- a + b - c
  Gamma <- dnorm(d1) * exp(-div_yield*t)/ (s * Sigma * sqrt(t))
  ## if t is 0, the above is 0/0 = NaN but dnorm(d1) term goes to 0 faster
  ## so we set the result to 0 in this case
  Gamma <- ifelse(is.nan(Gamma), 0,  Gamma)
  Vega <- s * sqrt(t) * dnorm(d1) * exp(-div_yield*t) / s
  Vanna <- Vega * d2 / (s * Sigma * sqrt(t))
  Volga <- Vega * d1 * d2 / (Sigma)
  callRho <- X * t * exp(-r * t) * Nd2
  putRho <- -X * t * exp(-r * t) * Nminusd2
  callProb <- Nd2
  putProb <- Nminusd2
  Greeks <- list(callDelta=callDelta, putDelta=putDelta, callTheta=callTheta,
                 putTheta=putTheta, Gamma=Gamma, Vega=Vega, callRho=callRho, putRho=putRho, Vanna=Vanna, Volga=Volga)
  extra <- list(d1=d1, d2=d2, Nd1=Nd1, Nd2=Nd2, Nminusd1=Nminusd1,
                Nminusd2=Nminusd2, callProb=callProb, putProb=putProb)
  list(call=call, put=put, Greeks=Greeks, extra=extra)
}

##' Generalized Black Scholes model implied volatility
GenBSImplied <- function(s, X, r, price, t, div_yield, PutOpt=FALSE,
                         toler=1e-6, max.iter=100, convergence=1e-8){
  ## discount the exercise price to eliminate r
  X = X * exp(-r * t)
  ## adjust the spot price to eliminate div_yield
  s = s * exp(-div_yield * t)
  ## use put call parity to convert put option into call option
  if (PutOpt) price = price + (s - X)
  SminusX = s - X
  SplusX = s + X
  if (price < SminusX || price < 0 || price > s){
    warning("Implied volatility is undefined because price is outside theoretical bounds")
    return(NA)
  }
  if (price == SminusX || price == 0) 
    ## if price equals intrinsic value, volatility is zero
    return(0)
  if (X == 0 && price != s) {
    ## if x is 0, option price must equal stock price
    warning("Implied volatility is undefined because price is outside theoretical bounds")
    return(NA)
  }
  ## since price exceeds intrinsic value, 0 is a lower bound for the volatility
  ## we now seek an upper bound by doubling 100% volatility until the price is exceeded
  upper <- 100e-2
  while (GenBS(s, X, 0, upper, t, 0)$call < price && upper < .Machine$integer.max) upper <-  upper * 2
  if (upper > .Machine$integer.max) upper <-  Inf
  f <- function(sigma){
    temp <- GenBS(s, X, 0, sigma, t, 0)
    temp$call - price
  }
  res2 <- uniroot(f, c(0, upper), tol=convergence)
  if (abs(res2$f.root > toler)){
    warning("Error finding implied volatility")
    return (NA)                         
  }
  return (res2$root)
}
