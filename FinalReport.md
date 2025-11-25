Final Report
================
Kayla Howa
2025-11-25

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

The questions we are interested in looking into are: 1.How does
elevation impact fruit yield? 2.Do older trees prodcue more fruit than
younger trees?

## Hypothesis

We hypothesize that high elevation leads to an decrease in fruit yield
in apple trees. Additionally, we hypothesize that older trees will
produce more fruit than younger trees.

## Prediction

We believe that elevation will lead to a decrease in fruit yield due
less CO2 in the air for the tree to consume and that more mature trees
will produce more fruit because they rely on new growth less.

# METHODS & ANALYSIS

### Method 1

We have created multiple bar plots to aid in visualization of the spread
of the data. The bar plots are also useful to try to initially predict
whether any of the factors we are looking into affect harvest quantity.

### First Analysis

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

![](FinalReport_files/figure-gfm/first-analysis-1.png)<!-- -->

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

![](FinalReport_files/figure-gfm/first-analysis-2.png)<!-- -->

### Method 2

We have fit a simple linear regression model to the data to try and
determine how our factors may be impacting fruit yield. For this model,
y is the response variable (harvest amount) and x<sub>1</sub> and
x<sub>2</sub> are our covariate factors (tree elevation, and year
planted).

The final model statement for y is:

$$y = \beta_{0} + \beta_{1}x_{1} + \beta_{2}x_{2} + \epsilon$$
$$\epsilon \sim N(0,\sigma^2)$$

where $\beta$ describes the effects of each predictor on y and
$\epsilon$ are normally distributed residual error terms with a variance
of $\sigma^2$.

Using our model statement, we have run a linear model to determine if
any of our covariates are significantly impacting harvest amounts.

### Second Analysis

``` r
## Creating a linear model to try and determine whether pruning impacts apple harvest abundance
apple_lm <- lm(harvest ~ elevation + year, data = apples)
summary(apple_lm)
```

    ## 
    ## Call:
    ## lm(formula = harvest ~ elevation + year, data = apples)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.18226 -0.49522  0.02132  0.32567  1.39608 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)    4.568495   3.648287   1.252    0.213
    ## elevation5500 -0.522197   0.714715  -0.731    0.466
    ## elevation5550  0.701397   0.508293   1.380    0.170
    ## elevation5580  0.455989   0.507709   0.898    0.371
    ## elevation5584  0.916571   0.591439   1.550    0.124
    ## elevation5585  0.482396   0.714537   0.675    0.501
    ## elevation5600  0.121520   0.433928   0.280    0.780
    ## elevation5604 -0.479334   0.532918  -0.899    0.370
    ## elevation5616 -0.583429   0.721002  -0.809    0.420
    ## elevation5633  0.482396   0.583540   0.827    0.410
    ## elevation5663 -0.038541   0.429217  -0.090    0.929
    ## elevation5700  0.173914   0.437671   0.397    0.692
    ## elevation5750  0.510716   0.583307   0.876    0.383
    ## elevation5770  0.474742   0.533194   0.890    0.375
    ## elevation5784  0.022197   0.583759   0.038    0.970
    ## elevation5900 -0.530616   0.584290  -0.908    0.366
    ## elevation6000  0.076169   0.462092   0.165    0.869
    ## elevation6080  0.021431   0.583719   0.037    0.971
    ## elevation6250  0.485457   0.714440   0.679    0.498
    ## elevation6500  0.042714   0.456137   0.094    0.926
    ## elevation6692  0.019135   0.583607   0.033    0.974
    ## elevation6700 -0.500153   0.487915  -1.025    0.307
    ## elevation6710  0.113854   0.536002   0.212    0.832
    ## elevation6720  0.416571   0.721002   0.578    0.565
    ## elevation7162 -0.561998   0.717979  -0.783    0.435
    ## elevation7200  0.459434   0.715840   0.642    0.522
    ## year          -0.001531   0.001808  -0.847    0.399
    ## 
    ## Residual standard error: 0.5832 on 117 degrees of freedom
    ##   (97 observations deleted due to missingness)
    ## Multiple R-squared:  0.2273, Adjusted R-squared:  0.05555 
    ## F-statistic: 1.324 on 26 and 117 DF,  p-value: 0.1582

``` r
elevation_lm <- lm(harvest ~ elevation, data = apples)
summary(elevation_lm)
```

    ## 
    ## Call:
    ## lm(formula = harvest ~ elevation, data = apples)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2500 -0.5000  0.0000  0.3125  1.3030 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.500e+00  4.119e-01   3.642 0.000403 ***
    ## elevation5500 -5.000e-01  7.134e-01  -0.701 0.484750    
    ## elevation5550  7.500e-01  5.044e-01   1.487 0.139730    
    ## elevation5580  5.000e-01  5.044e-01   0.991 0.323610    
    ## elevation5584  1.000e+00  5.825e-01   1.717 0.088634 .  
    ## elevation5585  5.000e-01  7.134e-01   0.701 0.484750    
    ## elevation5600  1.970e-01  4.242e-01   0.464 0.643238    
    ## elevation5604 -5.000e-01  5.317e-01  -0.940 0.348961    
    ## elevation5616 -5.000e-01  7.134e-01  -0.701 0.484750    
    ## elevation5633  5.000e-01  5.825e-01   0.858 0.392404    
    ## elevation5663 -4.167e-02  4.287e-01  -0.097 0.922735    
    ## elevation5700  1.875e-01  4.369e-01   0.429 0.668555    
    ## elevation5750  5.000e-01  5.825e-01   0.858 0.392404    
    ## elevation5770  5.000e-01  5.317e-01   0.940 0.348961    
    ## elevation5784 -9.197e-16  5.825e-01   0.000 1.000000    
    ## elevation5900 -5.000e-01  5.825e-01  -0.858 0.392404    
    ## elevation6000  2.059e-01  4.354e-01   0.473 0.637205    
    ## elevation6080 -8.209e-16  5.825e-01   0.000 1.000000    
    ## elevation6250  5.000e-01  7.134e-01   0.701 0.484750    
    ## elevation6500  5.556e-02  4.553e-01   0.122 0.903099    
    ## elevation6692 -8.854e-16  5.825e-01   0.000 1.000000    
    ## elevation6700 -5.000e-01  4.873e-01  -1.026 0.306989    
    ## elevation6710  1.667e-01  5.317e-01   0.313 0.754493    
    ## elevation6720  5.000e-01  7.134e-01   0.701 0.484750    
    ## elevation7162 -5.000e-01  7.134e-01  -0.701 0.484750    
    ## elevation7200  5.000e-01  7.134e-01   0.701 0.484750    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5825 on 118 degrees of freedom
    ##   (97 observations deleted due to missingness)
    ## Multiple R-squared:  0.2225, Adjusted R-squared:  0.05782 
    ## F-statistic: 1.351 on 25 and 118 DF,  p-value: 0.1444

``` r
year_lm <- lm(harvest ~ year, data = apples)
summary(year_lm)
```

    ## 
    ## Call:
    ## lm(formula = harvest ~ year, data = apples)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8323 -0.5566  0.1677  0.4312  1.4673 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  6.739943   2.430087   2.774  0.00629 **
    ## year        -0.002583   0.001229  -2.102  0.03728 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.593 on 142 degrees of freedom
    ##   (97 observations deleted due to missingness)
    ## Multiple R-squared:  0.03019,    Adjusted R-squared:  0.02336 
    ## F-statistic:  4.42 on 1 and 142 DF,  p-value: 0.03728

### Method 3

### Third Analysis

# DISCUSSION

### Interpretation - First Analysis

### Interpretation - Second Analysis

### Interpretation - Third Analysis

### Limitations

There are a few limitations that come with this data set. The first one
is that apple trees may produce a heavy crop of fruit in one year and
then very little to almost none the next. This is called “alternate” or
“biennial bearing”. The typical pattern is 2 alternating years, but some
trees may have an excessive crop one year and then very little for the
next two years (University of Saskatchewan, 2018). Without knowledge
about what part of this alternate bearing cycle the dataset trees are
limits us to not be able to make causation claims with any of our tested
factors.

The other limitation is with the dataset itself. There are many
observations that are missing data within rows. The amount of data is
inconsistent. Additonanlly, the values in the data set are poorly
labeled. For example, harvest amount in the dataset is labeled as
“normal”, “minimal”, and “above average”. We don’t have a reference
about what any of these are supposed to suggest or what the harvest
amounts are being compared to.

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

University of Saskatchewan. (2018, February 2). Alternate or biennial
bearing on fruit trees. College of Agriculture and bioresources.
<https://gardening.usask.ca/articles-and-lists/articles-disorders/disorder-alternate-or-biennial-bearing-on-fruit-trees.php>

Wikimedia Foundation. (2025, October 22). Dummy Variable (statistics).
Wikipedia. <https://en.wikipedia.org/wiki/Dummy_variable_(statistics)>
