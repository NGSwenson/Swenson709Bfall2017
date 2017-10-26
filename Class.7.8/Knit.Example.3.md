# Post-Hurricane Dynamics of Tropical Tree Functional Diversity

---

# Author: [Nate Swenson](http://www.swensonlab.com)

A cleaner way to link to an awesome [paper][1] or a far less interesting [paper][2]

[1]: http://onlinelibrary.wiley.com/doi/10.1890/11-0402.1/full
[2]: http://rspb.royalsocietypublishing.org/content/278/1707/877



## A simple header
### Tertiary header
#### Fourth ####
##### Fifth #####
###### Sixth ######

This code generates 50 random numbers from a normal distribution with a mean of zero and a s.d. of 1.

I *love* this code. **Are you listening?**

* First bullet point
	* Sub bullet point
* Second bullet point


```r
rand.vals = rnorm(50)
```

Here we plot a histogram of the 50 random numbers.


```r
hist(rand.vals, col="red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
Here we are generating 50 random numbers from a normal distribution, but because we set eval=F it does not run the R code and causes downstream calculations to fail.


```r
rand.vals.2 = rnorm(50)
```

Trying to plot a histogram of rand.vals.2


```r
hist(rand.vals.2, col="red")
```

```
## Error in hist(rand.vals.2, col = "red"): object 'rand.vals.2' not found
```

Here we make 50 random values and plot them against original 50 random values. Setting figure size to a 2 inch by 3 inch size.


```r
rand.vals.3 = rnorm(50)

plot(rand.vals.3~rand.vals, xlab="First dataset", ylab = "Third dataset", pch=21, bg="orange",cex=2,xlim=c(-5,5),ylim=c(-5,5))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


Here we make 50 random values and plot them against original 50 random values. Setting figure size to a 5 inch by 5 inch size.


```r
plot(rand.vals.3~rand.vals, xlab="First dataset", ylab = "Third dataset", pch=21, bg="orange",cex=2,xlim=c(-5,5),ylim=c(-5,5))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
