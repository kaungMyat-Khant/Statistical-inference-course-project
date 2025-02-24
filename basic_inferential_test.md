---
title: "The Effect of Vitamin C on Tooth Growth in Guinea Pigs:Basic Inferential Analysis"
author: "Kaung Myat Khant"
date: "2025-02-23"
output: 
  pdf_document:
    keep_md: TRUE
---



## Overview

The effect of vitamin C on tooth growth was examined in the guinea pigs. The length of odontoblasts were compared between two groups, vitamin C and orange juice. The tooth growth was also compared between vitamin C or orange juice doses of 0.5, 1 and 2 mg.

## Introduction

Odontoblasts are responsible for the growth of tooth in animals.Vitamin C (Ascorbic acid) is believed to have effect on tooth growth. In this study, we compare the length of tooth growth in guinea pigs that received one of the two vitamin C supplements, ascorbic acid(pure vitamin C) and orange juice. The effect of vitamin C dosage level are also compared.

## Methods

The data were from ToothGrowth data in R datasets package. The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC). The data were summarized using the five-point summary measures of central tendencies (mean/median) and spread (Standard deviation/ Interquartile range). The effect of the supplements and dosage levels on tooth growth were compare using Student t tests and presented in 95% confidence intervals.


``` r
library(datasets)
data <- ToothGrowth
dim(data) # 60 observations with three variables
```

```
## [1] 60  3
```

``` r
sum(is.na(data)) # There were no missing values
```

```
## [1] 0
```

``` r
head(data) # see the data
```

```
##    len supp dose
## 1  4.2   VC  0.5
## 2 11.5   VC  0.5
## 3  7.3   VC  0.5
## 4  5.8   VC  0.5
## 5  6.4   VC  0.5
## 6 10.0   VC  0.5
```

## Results

### Exploratory data analysis of length of growth by supplements


``` r
library(ggplot2)
ggplot(data, aes(x = supp, y = len))+
    geom_boxplot()+
    labs(y = "Length of odontoblasts(mm)", x = "VC = Ascorbic acid, OJ = Orange juice")
```

![Box-plot comparing the two supplements](basic_inferential_test_files/figure-latex/boxSupp-1.pdf) 

The box-plot showed that the orange juice supplement (OJ) had longer length of odontoblasts than the ascorbic acid form.

### Summary of length of growth by supplements


``` r
library(dplyr)
data %>% 
    group_by(supp) %>% 
    summarise(
        Mean = mean(len),
        SD = sd(len),
        Median = median(len),
        Q1 = quantile(len, probs = 0.25),
        Q3 = quantile(len, probs = 0.75),
        Min = min(len),
        Max = max(len)
    ) %>% 
    ungroup() %>% 
    knitr::kable(col.names = c("Supplements","Mean","SD","Median","Q1","Q3","Min","Max"),
                 digits = 2,
                 caption = "Summary of tooth growth by supplements")
```



Table: Summary of tooth growth by supplements

|Supplements |  Mean|   SD| Median|    Q1|    Q3| Min|  Max|
|:-----------|-----:|----:|------:|-----:|-----:|---:|----:|
|OJ          | 20.66| 6.61|   22.7| 15.52| 25.73| 8.2| 30.9|
|VC          | 16.96| 8.27|   16.5| 11.20| 23.10| 4.2| 33.9|

**Table 1** shows the summary of the data by supplement types. The growth for orange juice was 21mm (SD =6.6) and that of ascorbic acid form was 17mm (SD = 8.2).

### Comparison of tooth growth between the two supplement groups


``` r
tSupp <- with(data, t.test(len ~ supp, alternative = "greater",
                  paired = FALSE, var.equal = TRUE))
data.frame(meanDiff = tSupp$estimate[1] - tSupp$estimate[2],t = tSupp$statistic, df =tSupp$parameter, CI = paste0(round(tSupp$conf.int[1],2),",",round(tSupp$conf.int[2],2)),pValue = tSupp$p.value, row.names = NULL) %>% knitr::kable(
    col.names = c("Mean difference","t-value","degree of freedom","95% CI","P value"),
    digits = 4,
    caption = "T test results for comparison of growth between two supplements"
)
```



Table: T test results for comparison of growth between two supplements

| Mean difference| t-value| degree of freedom|95% CI   | P value|
|---------------:|-------:|-----------------:|:--------|-------:|
|             3.7|  1.9153|                58|0.47,Inf |  0.0302|

**Table 2** showed the student t test results of comparison between two supplements. Under the null hypothesis of no true difference in means between orange juice and vitamin C with 5% significant level, we have sufficient data to reject the null hypothesis. Thus, we can conclude that orange juice has more effect on tooth growth than vitamin C.

### Exploratoroy data analysis of length of growth by dosage levels


``` r
library(ggplot2)
ggplot(data, aes(x = factor(dose), y = len))+
    geom_boxplot()+
    labs(y = "Length of odontoblasts(mm)", x = "Dose level(mg/day)")
```

![Box-plot comparing the three dosage levels](basic_inferential_test_files/figure-latex/boxDose-1.pdf) 

The box-plot showed that the higher the dosage, the higher the length.

### Summary of length of growth by dose levels


``` r
library(dplyr)
data %>% 
    group_by(factor(dose)) %>% 
    summarise(
        Mean = mean(len),
        SD = sd(len),
        Median = median(len),
        Q1 = quantile(len, probs = 0.25),
        Q3 = quantile(len, probs = 0.75),
        Min = min(len),
        Max = max(len)
    ) %>% 
    ungroup() %>% 
    knitr::kable(col.names = c("Dose levels","Mean","SD","Median","Q1","Q3","Min","Max"),
                 digits = 2,
                 caption = "Summary of tooth growth by dose levels")
```



Table: Summary of tooth growth by dose levels

|Dose levels |  Mean|   SD| Median|    Q1|    Q3|  Min|  Max|
|:-----------|-----:|----:|------:|-----:|-----:|----:|----:|
|0.5         | 10.61| 4.50|   9.85|  7.22| 12.25|  4.2| 21.5|
|1           | 19.74| 4.42|  19.25| 16.25| 23.38| 13.6| 27.3|
|2           | 26.10| 3.77|  25.95| 23.53| 27.83| 18.5| 33.9|

**Table 3** shows the summary of the data by dose levels. The growth for 0.5mg/day, 1mg/day and 2mg/day were 11mm (SD = 4.5), 20mm (SD = ) and 26mm (SD=3.8), respectively.

### Comparision of tooth growth among the dose levels


``` r
t.test(
    x = data[data$dose==0.5, "len"],
    y = data[data$dose==1, "len"],
    paired = FALSE,
    var.equal = TRUE,
    alternative = "less"
)
```

```
## 
## 	Two Sample t-test
## 
## data:  data[data$dose == 0.5, "len"] and data[data$dose == 1, "len"]
## t = -6.4766, df = 38, p-value = 6.331e-08
## alternative hypothesis: true difference in means is less than 0
## 95 percent confidence interval:
##       -Inf -6.753344
## sample estimates:
## mean of x mean of y 
##    10.605    19.735
```

The test results showed that we can reject the null hypothesis of no true difference between dose level 0.5 mg/day and 1 mg/day with 5% significant level. Thus, we can conclude that dose level 1mg/day had higher growth than the 0.5mg/day (P value \< 0.001).


``` r
t.test(
    x = data[data$dose==0.5, "len"],
    y = data[data$dose==2, "len"],
    paired = FALSE,
    var.equal = TRUE,
    alternative = "less"
)
```

```
## 
## 	Two Sample t-test
## 
## data:  data[data$dose == 0.5, "len"] and data[data$dose == 2, "len"]
## t = -11.799, df = 38, p-value = 1.419e-14
## alternative hypothesis: true difference in means is less than 0
## 95 percent confidence interval:
##       -Inf -13.28093
## sample estimates:
## mean of x mean of y 
##    10.605    26.100
```

The test results showed that we can reject the null hypothesis of no true difference between dose level 0.5 mg/day and 2 mg/day with 5% significant level. Thus, we can conclude that dose level 2mg/day had higher growth than the 0.5mg/day (P value \< 0.001).


``` r
t.test(
    x = data[data$dose==1, "len"],
    y = data[data$dose==2, "len"],
    paired = FALSE,
    var.equal = TRUE,
    alternative = "less"
)
```

```
## 
## 	Two Sample t-test
## 
## data:  data[data$dose == 1, "len"] and data[data$dose == 2, "len"]
## t = -4.9005, df = 38, p-value = 9.054e-06
## alternative hypothesis: true difference in means is less than 0
## 95 percent confidence interval:
##       -Inf -4.175196
## sample estimates:
## mean of x mean of y 
##    19.735    26.100
```

The test results showed that we can reject the null hypothesis of no true difference between dose level 1 mg/day and 2 mg/day with 5% significant level. Thus, we can conclude that dose level 2mg/day had higher growth than the 1mg/day (P value \< 0.001).


``` r
p.adjust(c(6.342e-08, 2.199e-14, 9.532e-06), method = "bonf")
```

```
## [1] 1.9026e-07 6.5970e-14 2.8596e-05
```

The adjusted p values with Bonferroni's adjustment are also very small.


``` r
aov <- aov(len ~ dose, data)
summary(aov)
```

```
##             Df Sum Sq Mean Sq F value   Pr(>F)    
## dose         1   2224  2224.3   105.1 1.23e-14 ***
## Residuals   58   1228    21.2                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
oneway.test(len ~ dose, data, var.equal = TRUE)
```

```
## 
## 	One-way analysis of means
## 
## data:  len and dose
## F = 67.416, num df = 2, denom df = 57, p-value = 9.533e-16
```

``` r
pairwise.t.test(data$len, factor(data$dose), p.adjust.method = "bonf", paired = FALSE, alternative = "greater")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  data$len and factor(data$dose) 
## 
##   0.5     1      
## 1 1.0e-08 -      
## 2 < 2e-16 2.2e-05
## 
## P value adjustment method: bonferroni
```

Analysis of variance also showed the same results.

## Conclusion

We can conclude that the orange juice (natural supplement) had greater effect on tooth growth than the ascorbic acid form of vitamin C. Also, the higher the vitamin C doses, the longer the length of odontoblasts.

### Assumptions

In our analysis, we have the following assumptions on our data:

-   The data were independent and identically distributed.

-   The data were normally distributed across each group.

-   The variance in each group were homogeneous


``` r
shapiro.test(data$len)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  data$len
## W = 0.96743, p-value = 0.1091
```

``` r
shapiro.test(data[data$supp=="OJ", "len"])
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  data[data$supp == "OJ", "len"]
## W = 0.91784, p-value = 0.02359
```

``` r
shapiro.test(data[data$supp=="VC", "len"])
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  data[data$supp == "VC", "len"]
## W = 0.96567, p-value = 0.4284
```

``` r
shapiro.test(data[data$dose==0.5, "len"])
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  data[data$dose == 0.5, "len"]
## W = 0.94065, p-value = 0.2466
```

``` r
shapiro.test(data[data$dose==1, "len"])
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  data[data$dose == 1, "len"]
## W = 0.93134, p-value = 0.1639
```

``` r
shapiro.test(data[data$dose==2, "len"])
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  data[data$dose == 2, "len"]
## W = 0.97775, p-value = 0.9019
```

Testing the normality with Shapiro-Wilk normality test at 1% significant level, the data were normally distributed across each group.


``` r
var.test(data[data$supp=="OJ", "len"],data[data$supp=="VC", "len"])
```

```
## 
## 	F test to compare two variances
## 
## data:  data[data$supp == "OJ", "len"] and data[data$supp == "VC", "len"]
## F = 0.6386, num df = 29, denom df = 29, p-value = 0.2331
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.3039488 1.3416857
## sample estimates:
## ratio of variances 
##          0.6385951
```

``` r
var.test(data[data$dose==0.5, "len"],data[data$dose==1, "len"])
```

```
## 
## 	F test to compare two variances
## 
## data:  data[data$dose == 0.5, "len"] and data[data$dose == 1, "len"]
## F = 1.0386, num df = 19, denom df = 19, p-value = 0.9351
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.4110751 2.6238736
## sample estimates:
## ratio of variances 
##           1.038561
```

``` r
var.test(data[data$dose==0.5, "len"],data[data$dose==2, "len"])
```

```
## 
## 	F test to compare two variances
## 
## data:  data[data$dose == 0.5, "len"] and data[data$dose == 2, "len"]
## F = 1.4215, num df = 19, denom df = 19, p-value = 0.4505
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.5626393 3.5913009
## sample estimates:
## ratio of variances 
##           1.421481
```

``` r
var.test(data[data$dose==2, "len"],data[data$dose==1, "len"])
```

```
## 
## 	F test to compare two variances
## 
## data:  data[data$dose == 2, "len"] and data[data$dose == 1, "len"]
## F = 0.73062, num df = 19, denom df = 19, p-value = 0.5005
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.289188 1.845874
## sample estimates:
## ratio of variances 
##          0.7306192
```

Comparing the variance across each group using F test at 5% significant level, there is homogeneity of variance across each groups.

Since the assumptions were met, we can conclude that our findings are robust and orange juice with higher dose of vitamin C had greater effect on tooth growth than ascorbic acid form. Orange juice could be a good nutrient for tooth growth.
