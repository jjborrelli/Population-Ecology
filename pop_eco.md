Population Ecology
========================================================



```r
N.t = 30
R = 1.16

N.t1 <- N.t * R
```


You can also embed plots, for example:


```r
N <- c()
for (i in 1:10) {
    N[i] <- N.t * R^i
}

plot(N)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


