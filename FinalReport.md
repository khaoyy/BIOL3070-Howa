Final Report
================
Kayla Howa
2025-11-13

# ABSTRACT

# BACKGROUND

Apples held the top spot for total fruit available for consumption in
2021 (Kantor and Blazejczyk, 2023) and, according to USApple’s analysis,
total U.S. apple production for the 2025/26 crop year (CY) is forecast
at 11.7 billion pounds / 278.5 million bushels (USApple, 2025). 

With apples being such a hot commodity, it is important to figure out
what factors may be influencing harvest sizes to ensure bountiful
harvests season after season. We have been provided a dataset from the
Wasatch Back Fruit Tree Project, a citizen led research project through
Utah State University Extension. Their goal is to assemble data on the
fruit tree varieties that can reliably produce on the Wasatch Front
(About the Wasatch, n.d.).

# STUDY QUESTION & HYPOTHESIS

## Questions

1.  Does pruning trees lead to an increase in fruit yield in apple
    trees?
2.  Does elevation play a role in fruit yield?
3.  Do older trees produce more fruit?

## Hypothesis

We hypothesize that pruning trees leads to an increase in fruit yield in
apple trees. Additionally, we hypothesize that older trees will produce
more fruit than younger trees, and that elevation negatively impacts
harvest size.

## Prediction

I predict that by pruning fruit bearing trees, such as apples, the fruit
yield will increase for the following harvest season. Additonally, we
predict that trees that are at a higher elevation will produce less
bountiful harvests and that older trees will produce more fruit than
younger trees.

# METHODS

### Method 1

We have fit a simple linear model to the data, with y being the response
variable (harvest amount) and x<sub>1</sub>, x<sub>2</sub>, and
x<sub>3</sub> being our covariate factors (whether the tree was pruned,
tree elevation, and year planted). Because the pruned covariate is
categorical, dummy variables were automatically created for it. One
pruned category is left out in order to prevent collinearity among the
data.

The final model statement for y is:

$$y = \beta_{0} + \beta_{1}x_{1} + \beta_{2}x_{2} + \beta_{3}x_{3} + \epsilon$$
$$\epsilon \sim N(0,\sigma^2)$$

where $\beta$ describes the effects of each predictor on y and
$\epsilon$ are normally distributed residual error terms with a variance
of $\sigma^2$.

### Method 2

We performed an ANOVA on our linear model to determine whether or not
there is a potential significant effect of any covariates on our
response, y.

### Method 3

We have created multiple bar plots to aid in visualization of the spread
of the data.

### Analysis

## First Analysis

``` r
##Histograms
hist(apples$harvest)
```

<img src="FinalReport_files/figure-gfm/first-analysis-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
hist(apples$year)
```

<img src="FinalReport_files/figure-gfm/first-analysis-2.png" style="display: block; margin: auto auto auto 0;" />

``` r
hist(as.numeric(apples$elevation))
```

<img src="FinalReport_files/figure-gfm/first-analysis-3.png" style="display: block; margin: auto auto auto 0;" />

``` r
## Create scatterplot matrix to determine relationships
plot(~ pruned + harvest + elevation + year, data = apples)
```

<img src="FinalReport_files/figure-gfm/first-analysis-4.png" style="display: block; margin: auto auto auto 0;" />

``` r
##Create scatterplot to see relationships
plot(apples$harvest, apples$pruned)
```

<img src="FinalReport_files/figure-gfm/first-analysis-5.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(apples$harvest, apples$elevation)
```

<img src="FinalReport_files/figure-gfm/first-analysis-6.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(apples$harvest, apples$year)
```

<img src="FinalReport_files/figure-gfm/first-analysis-7.png" style="display: block; margin: auto auto auto 0;" />

``` r
## Creating a linear model to try and determine whether pruning impacts apple harvest abundance
apple_lm <- lm(harvest ~ pruned + elevation + year, data = apples)
summary(apple_lm)
```

    ## 
    ## Call:
    ## lm(formula = harvest ~ pruned + elevation + year, data = apples)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.02866 -0.45672  0.00786  0.37053  1.42281 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)    3.1553437  3.7225603   0.848    0.398
    ## prunedY       -0.2500719  0.1520753  -1.644    0.103
    ## elevation5500 -0.6361058  0.7129397  -0.892    0.374
    ## elevation5550  0.6632429  0.5051644   1.313    0.192
    ## elevation5580  0.5405692  0.5066688   1.067    0.288
    ## elevation5584  0.8333568  0.5893551   1.414    0.160
    ## elevation5585  0.6162564  0.7140447   0.863    0.390
    ## elevation5600  0.1934423  0.4330166   0.447    0.656
    ## elevation5604 -0.3646577  0.5336553  -0.683    0.496
    ## elevation5616 -0.6666432  0.7175944  -0.929    0.355
    ## elevation5633  0.6162564  0.5850278   1.053    0.294
    ## elevation5663  0.0745083  0.4316352   0.173    0.863
    ## elevation5700  0.2901310  0.4402287   0.659    0.511
    ## elevation5750  0.6303800  0.5836593   1.080    0.282
    ## elevation5770  0.3623673  0.5337460   0.679    0.499
    ## elevation5784  0.1361058  0.5836787   0.233    0.816
    ## elevation5900 -0.5152687  0.5801560  -0.888    0.376
    ## elevation6000  0.0308665  0.4595898   0.067    0.947
    ## elevation6080  0.0106881  0.5795507   0.018    0.985
    ## elevation6250  0.3677114  0.7128990   0.516    0.607
    ## elevation6500  0.0074725  0.4533579   0.016    0.987
    ## elevation6692  0.1345789  0.5836411   0.231    0.818
    ## elevation6700 -0.6251123  0.4903242  -1.275    0.205
    ## elevation6710  0.0152922  0.5355056   0.029    0.977
    ## elevation6720  0.3333568  0.7175944   0.465    0.643
    ## elevation7162 -0.6559551  0.7150935  -0.917    0.361
    ## elevation7200  0.6048049  0.7161604   0.845    0.400
    ## year          -0.0007634  0.0018550  -0.412    0.681
    ## 
    ## Residual standard error: 0.579 on 116 degrees of freedom
    ##   (97 observations deleted due to missingness)
    ## Multiple R-squared:  0.2449, Adjusted R-squared:  0.06911 
    ## F-statistic: 1.393 on 27 and 116 DF,  p-value: 0.1165

``` r
appleResids <- apple_lm$residuals
qqnorm(appleResids)
```

<img src="FinalReport_files/figure-gfm/first-analysis-8.png" style="display: block; margin: auto auto auto 0;" />

``` r
shapiro.test(appleResids)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  appleResids
    ## W = 0.95305, p-value = 8.429e-05

## Second Analysis

``` r
## ANOVA
options(max.print=1000000)
appleANOVA <- aov(apple_lm)
summary(appleANOVA)
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)
    ## pruned        1   0.85  0.8452   2.521  0.115
    ## elevation    25  11.71  0.4683   1.397  0.120
    ## year          1   0.06  0.0568   0.169  0.681
    ## Residuals   116  38.88  0.3352               
    ## 97 observations deleted due to missingness

## Third Analysis

``` r
## Pruned Plots
countsPruned <- table(apples$harvest, apples$pruned)
summary(countsPruned)
```

    ## Number of cases in table: 144 
    ## Number of factors: 2 
    ## Test for independence of all factors:
    ##  Chisq = 2.3737, df = 2, p-value = 0.3052
    ##  Chi-squared approximation may be incorrect

``` r
print(countsPruned)
```

    ##    
    ##      N  Y
    ##   1 22 40
    ##   2 34 39
    ##   3  5  4

``` r
barplot(countsPruned, main = "Distribution of Harvest Quantity by Tree Pruning", 
        cex.sub = 0.75, sub = "Total Number of Observations = 242",xlab = "Was the Tree Pruned?", 
        ylab = "Counts", ylim = c(0,100),col = c("#117733", "#44AA99", "#88CCEE"))
```

![](FinalReport_files/figure-gfm/third-analysis-1.png)<!-- -->

``` r
##Elevation Plots
countsElevation <- table(apples$harvest, apples$elevation)
summary(countsElevation)
```

    ## Number of cases in table: 144 
    ## Number of factors: 2 
    ## Test for independence of all factors:
    ##  Chisq = NaN, df = 56, p-value = NA
    ##  Chi-squared approximation may be incorrect

``` r
print(countsElevation)
```

    ##    
    ##     5400 5500 5550 5580 5584 5585 5600 5604 5616 5633 5663 5700 5750 5770 5784
    ##   1    1    1    1    1    0    0   14    3    1    0   13    5    1    0    1
    ##   2    1    0    1    2    1    1   15    0    0    2   11   11    0    3    1
    ##   3    0    0    2    1    1    0    4    0    0    0    0    0    1    0    0
    ##    
    ##     5800 5900 6000 6080 6250 6500 6692 6700 6710 6720 6722 7000 7162 7200
    ##   1    0    2    5    1    0    4    1    5    1    0    0    0    1    0
    ##   2    0    0   12    1    1    5    1    0    2    1    0    0    0    1
    ##   3    0    0    0    0    0    0    0    0    0    0    0    0    0    0

``` r
barplot(countsElevation, main = "Distribution of Harvest Quantity by Elevation", 
        cex.sub = 0.75, sub = "Total Number of Observations = 144",xlab = "Was the Tree Pruned?", 
        ylab = "Counts", ylim = c(0,100),col = c("#117733", "#44AA99", "#88CCEE"))
```

![](FinalReport_files/figure-gfm/third-analysis-2.png)<!-- -->

``` r
##Year Plots
countsYear <- table(apples$harvest, apples$year)
summary(countsYear)
```

    ## Number of cases in table: 144 
    ## Number of factors: 2 
    ## Test for independence of all factors:
    ##  Chisq = NaN, df = 82, p-value = NA
    ##  Chi-squared approximation may be incorrect

``` r
print(countsYear)
```

    ##    
    ##     1900 1905 1924 1930 1935 1940 1949 1950 1964 1970 1974 1977 1978 1980 1984
    ##   1    3    0    7    1    0    0    1    5    1    0    0    1    0    2    1
    ##   2   13    0    1    1    1    1    0    9    0    3    0    1    1    3    0
    ##   3    0    2    0    0    0    0    0    2    0    0    1    0    0    0    0
    ##    
    ##     1985 1990 1992 1993 1995 1999 2000 2002 2003 2005 2006 2007 2008 2010 2013
    ##   1    0    1    0    0    3    0    0    5    0    2    3    1    6    1    0
    ##   2    0    1    0    3    6    1    0   10    1    4    0    1    0    2    0
    ##   3    0    0    1    1    0    0    1    0    0    0    0    0    0    0    0
    ##    
    ##     2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2028
    ##   1    0    2    1    2    4    4    1    1    0    3    0    0
    ##   2    1    2    0    0    1    3    0    0    0    0    2    1
    ##   3    0    0    1    0    0    0    0    0    0    0    0    0

``` r
barplot(countsYear, main = "Distribution of Harvest Quantity by Elevation", 
        cex.sub = 0.75, sub = "Total Number of Observations = 144",xlab = "Was the Tree Pruned?", 
        ylab = "Counts", ylim = c(0,100),col = c("#117733", "#44AA99", "#88CCEE"))
```

![](FinalReport_files/figure-gfm/third-analysis-3.png)<!-- -->

# DISCUSSION

### Interpretation - First Analysis

### Interpretation - Second Analysis

### Limitations

Issues with Alternate bearing in addition to a lack of year to year
data.

Missing data within rows.

Poorly labeled values -\> what does ‘normal’ ‘above average’ and
‘minimal’ mean? -\> what are they comparing it to? -\> what does it
mean?

# CONCLUSION

# REFERENCES

2025-26 apple production will reach nearly 279 million bushels. USApple.
(2025, August 15).
<https://usapple.org/news-resources/2025-26-apple-crop-outlook>

About the Wasatch Back Fruit Tree Project. Utah State University.
(n.d.). <https://extension.usu.edu/wasatch/WBFTP_about>

Kantor, L., & Blazejczyk, A. (2023, May 5). Apples and oranges are the
top U.S. fruit choices. Apples and oranges are the top U.S. fruit
choices \| Economic Research Service.
<https://www.ers.usda.gov/data-products/chart-gallery/chart-detail?chartId=58322>
