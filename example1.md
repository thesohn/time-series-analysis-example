code1
================
Anthony Sohn
June 27, 2016

``` r
######################################################################
library(TSA)
```

    ## Loading required package: leaps

    ## Loading required package: locfit

    ## locfit 1.5-9.1    2013-03-22

    ## Loading required package: mgcv

    ## Loading required package: nlme

    ## This is mgcv 1.8-12. For overview type 'help("mgcv-package")'.

    ## Loading required package: tseries

    ## 
    ## Attaching package: 'TSA'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     acf, arima

    ## The following object is masked from 'package:utils':
    ## 
    ##     tar

``` r
require(forecast)
```

    ## Loading required package: forecast

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: timeDate

    ## 
    ## Attaching package: 'timeDate'

    ## The following objects are masked from 'package:TSA':
    ## 
    ##     kurtosis, skewness

    ## This is forecast 7.1

    ## 
    ## Attaching package: 'forecast'

    ## The following objects are masked from 'package:TSA':
    ## 
    ##     fitted.Arima, plot.Arima

    ## The following object is masked from 'package:nlme':
    ## 
    ##     getResponse

``` r
data(co2)
data = co2

######################################################################
#STEP 1: plot the data 

plot(data, ylab = "ppm",main = "Monthly CO2 levels at Alert, 
     Northwest Territories, Canada \n (Jan 1994 - Dec 2004)")
```

![](example1_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
######################################################################
#STEP 2: use automated algorithm to find useful seasonal ARIMA model

fit1 = auto.arima(data)

######################################################################
#STEP 3a: Plot ACF and PACF of the residuals  of model

y= fit1$residuals
plot(y, main = "Plot of Residuals", ylab = "Residual")
```

![](example1_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
acf(as.vector(y), na.action = na.pass, main = "Plot of ACF")
```

![](example1_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
#Analysis: In ACF plot, we do not observe any significant 
#correlations (2/12/2016 p.67)

pacf(as.vector(y), na.action = na.pass, main = "Plot of PACF")
```

![](example1_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
#Analysis:In PACF plot, we observe significant correlation at lag 18 but 
#we dismissed it as possible type 1 error (2/12/2016 p. 67)

#STEP 3b: 
#ljung-box test to check for dependency in time lag (2/12/2016 p. 67)
#null hypothesis: residuals are independent up to lag h
#alternative hypothesis: there is some dependence structure remaining

#we assign lag h = min(2d, n/5), where d = 12, n = 132 (2/12/2016 p. 67)
#therefore h = min(24, 26.4) = 24
Box.test(y, lag =24, type = "Ljung-Box" )
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  y
    ## X-squared = 25.4, df = 24, p-value = 0.3843

``` r
#test result: since the p-value is not significant, we cannot reject the 
#null hypothesis that the noise is independent

######################################################################
#STEP 5: check if residuals look like white noise

plot(y, main = "Residuals")
```

![](example1_files/figure-markdown_github/unnamed-chunk-1-5.png)

``` r
#Analysis: yes, the residuals look like white noise


######################################################################
#STEP 6: Forecast
fc= forecast(fit1, 12)
plot(fc, main = "Forecast for 2005")
```

![](example1_files/figure-markdown_github/unnamed-chunk-1-6.png)

``` r
######################################################################
#STEP 7a: check residuals for normality using shapiro wilk

#null hypothesis: residuals are normal
#alternative hypothesis: residuals are not normal

shapiro.test(as.vector(y)) #2/29/2016 pg. 82
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  as.vector(y)
    ## W = 0.97874, p-value = 0.0364

``` r
# #the actual values
# lines(468:508, cmort[468:508], col="blue")
```
