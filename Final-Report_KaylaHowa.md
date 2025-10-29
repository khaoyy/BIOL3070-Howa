Final Report
================
Kayla Howa
2025-10-29

- [ABSTRACT](#abstract)
- [STUDY QUESTION & HYPOTHESIS](#study-question--hypothesis)
  - [Question](#question)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
  - [](#section)
- [METHODS](#methods)
  - [Method 1](#method-1)
  - [Method 2](#method-2)
  - [Method 3](#method-3)
  - [Analysis](#analysis)
  - [First Analysis](#first-analysis)
  - [Second Analysis](#second-analysis)
  - [Third Analysis](#third-analysis)
- [DISCUSSION](#discussion)
  - [Interpretation - First Analysis](#interpretation---first-analysis)
  - [Interpretation - Second
    Analysis](#interpretation---second-analysis)
  - [Limitations](#limitations)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

NA = 0 minimal = 1 normal = 2 above average = 3 prune = y no prune = n

# STUDY QUESTION & HYPOTHESIS

## Question

Does pruning trees lead to an increase in fruit yield in apple trees?

## Hypothesis

We hypothesize that pruning trees leads to an increase in fruit yield in
apple trees.

## Prediction

## 

# METHODS

### Method 1

### Method 2

### Method 3

### Analysis

## First Analysis

``` r
## one-sample t-test 
```

## Second Analysis

``` r
## Creating a linear model to try and determine whether pruning impacts apple harvest abundance
apple_lm <- lm(harvest ~ pruned, data = apples)
resids <- apple_lm$residuals
qqnorm(resids)
```

<img src="Final-Report_KaylaHowa_files/figure-gfm/second-analysis-1.png" style="display: block; margin: auto auto auto 0;" />

## Third Analysis

``` r
counts <- table(apples$harvest, apples$pruned)

barplot(counts, main = "Distribution of Harvest Quantity by Tree Pruning", 
        cex.sub = 0.75, sub = "Total Number of Observations = 242",xlab = "Was the Tree Pruned?", 
        ylab = "Counts", ylim = c(0,100),col = c("#117733", "#44AA99", "#88CCEE"))
```

![](Final-Report_KaylaHowa_files/figure-gfm/third-analysis-1.png)<!-- -->

# DISCUSSION

### Interpretation - First Analysis

### Interpretation - Second Analysis

### Limitations

# CONCLUSION

# REFERENCES
