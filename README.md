---
title: "R package predict3d"
author: "Keon-Woong Moon"
date: "2019-02-27"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{predict3d}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



R package `predict3d` aims to draw predicts plot for various regressiom models. The main two functions are ggPredict() for 2-dimensional plot and predict3d() for 3-dimensional plot.

## Package Installation

You can install `predict3d` package from github.


```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("cardiomoon/predict3d"")
```

## Linear Regression Models

You can draw linear regression models. First model has one categorial and one continuous explanatory variables.


```r
require(predict3d)
require(rgl)
fit1=lm(Sepal.Length~Sepal.Width*Species,data=iris)
fit1

Call:
lm(formula = Sepal.Length ~ Sepal.Width * Species, data = iris)

Coefficients:
                  (Intercept)                    Sepal.Width  
                       2.6390                         0.6905  
            Speciesversicolor               Speciesvirginica  
                       0.9007                         1.2678  
Sepal.Width:Speciesversicolor   Sepal.Width:Speciesvirginica  
                       0.1746                         0.2110  
```

You can draw plot for this model. ggPredict() function draws a scatterplot with regression line and shows regression equations parallel to the regression lines.


```r
ggPredict(fit1,digits=1)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
predict3d(fit1,radius=0.05)
Warning in Ops.factor(z[2], z[1]): '<' not meaningful for factors
rglwidget(elementId = "1st")
PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
Warning in normalizePath(f2): path[1]="./webshot179f1c38775b.png": No such
file or directory
Warning in file(con, "rb"): cannot open file './webshot179f1c38775b.png':
No such file or directory
Error in file(con, "rb"): cannot open the connection
```

The second model has two continuous variables as explanatory variables. You can change the labels and the relative x position and the y position.


```r
fit2=lm(mpg~wt*hp,data=mtcars)
ggPredict(fit2,labels=paste0("label",1:3),xpos=c(0.3,0.4,0.3))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
predict3d(fit2)
rglwidget(elementId = "2nd")
PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
Warning in normalizePath(f2): path[1]="./webshot179f1648fbebc.png": No such
file or directory
Warning in file(con, "rb"): cannot open file './webshot179f1648fbebc.png':
No such file or directory
Error in file(con, "rb"): cannot open the connection
```

## Generalized Linear Models

You can draw gemneralized linear models.


```r
require(TH.data)
fit3=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
ggPredict(fit3,se=TRUE,show.text = FALSE)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
predict3d(fit3,radius=0.5)
rglwidget(elementId = "3rd")
Error in file(con, "rb"): cannot open the connection
```

## Local Polynomial Regression Fitting

You can draw the loess model.


```r
fit=loess(mpg~hp*wt,data=mtcars)
ggPredict(fit)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
predict3d(fit,radius=2)
rglwidget(elementId = "4th")
PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
Warning in normalizePath(f2): path[1]="./webshot179f123ba1d6c.png": No such
file or directory
Warning in file(con, "rb"): cannot open file './webshot179f123ba1d6c.png':
No such file or directory
Error in file(con, "rb"): cannot open the connection
```

## Play with predict3d()

Once you have create a model with predict3d(), you can move your object with your mouse or R codes. For example, You can rotate you object with this R codes.


```r
start <- proc.time()[3]
while ((i <- 36*(proc.time()[3] - start)) < 360) {
     rgl.viewpoint(i, i/4); 
}
play3d(spin3d(axis = c(1, 0, 0), rpm = 30), duration = 2)
```

You can save your 3d plot as a figure file or pdf file.


```r
rgl.snapshot("fig1.png")
rgl.postscript("fig2.pdf")
```

For more information about package `rgl`, please read the package vignette at: https://CRAN.R-project.org/package=rgl/vignettes/rgl.htmll 

