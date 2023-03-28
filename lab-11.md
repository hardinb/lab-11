Lab 11 - Smoking during pregnancy
================
Ben Hardin
3/28/2023

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

``` r
set.seed(1234)
```

``` r
data(ncbirths)
```

### Exercise 1

There are 1000 cases (births) in our sample. For each birth, there are
13 variables.

1.  Father’s age (numerical)
2.  Mother’s age (numerical)
3.  Whether the mother was classified as “young” or “mature”
    (categorical)
4.  Length of pregnancy in weeks (numerical)
5.  Whether the birth was classified as premature or full-term
    (categorical)
6.  Number of hospital visits during pregnancy (numerical)
7.  Whether the mother was married or not married (categorical)
8.  Weight gained by the mother during pregnancy (numerical)
9.  Weight of the baby at birth (numerical)
10. Whether the baby was classified as low birthweight or not
    (categorical)
11. Gender of the baby (categorical)
12. Smoking status of the mother (categorical)
13. Whether the mother is white or not white (categorical)

There are a few outliers in our numerical data. In particular, there are
a lot of outliers for the babyweight variable, with a few babies with
very high birthweights and a lot of babies with very low birthweights.

``` r
tibble(ncbirths)
```

    ## # A tibble: 1,000 × 13
    ##     fage  mage mature   weeks premie visits marital gained weight lowbi…¹ gender
    ##    <int> <int> <fct>    <int> <fct>   <int> <fct>    <int>  <dbl> <fct>   <fct> 
    ##  1    NA    13 younger…    39 full …     10 not ma…     38   7.63 not low male  
    ##  2    NA    14 younger…    42 full …     15 not ma…     20   7.88 not low male  
    ##  3    19    15 younger…    37 full …     11 not ma…     38   6.63 not low female
    ##  4    21    15 younger…    41 full …      6 not ma…     34   8    not low male  
    ##  5    NA    15 younger…    39 full …      9 not ma…     27   6.38 not low female
    ##  6    NA    15 younger…    38 full …     19 not ma…     22   5.38 low     male  
    ##  7    18    15 younger…    37 full …     12 not ma…     76   8.44 not low male  
    ##  8    17    15 younger…    35 premie      5 not ma…     15   4.69 low     male  
    ##  9    NA    16 younger…    38 full …      9 not ma…     NA   8.81 not low male  
    ## 10    20    16 younger…    37 full …     13 not ma…     52   6.94 not low female
    ## # … with 990 more rows, 2 more variables: habit <fct>, whitemom <fct>, and
    ## #   abbreviated variable name ¹​lowbirthweight

``` r
ggplot(ncbirths, aes(y = fage))+
  geom_boxplot()
```

![](lab-11_files/figure-gfm/check-outliers-1.png)<!-- -->

``` r
ggplot(ncbirths, aes(y = mage))+
  geom_boxplot()
```

![](lab-11_files/figure-gfm/check-outliers-2.png)<!-- -->

``` r
ggplot(ncbirths, aes(y = weeks))+
  geom_boxplot()
```

![](lab-11_files/figure-gfm/check-outliers-3.png)<!-- -->

``` r
ggplot(ncbirths, aes(y = visits))+
  geom_boxplot()
```

![](lab-11_files/figure-gfm/check-outliers-4.png)<!-- -->

``` r
ggplot(ncbirths, aes(y = gained))+
  geom_boxplot()
```

![](lab-11_files/figure-gfm/check-outliers-5.png)<!-- -->

``` r
ggplot(ncbirths, aes(y = weight))+
  geom_boxplot()
```

![](lab-11_files/figure-gfm/check-outliers-6.png)<!-- -->

### Exercise 2

The average weight of (presumably) white babies is 7.25 pounds.

``` r
ncbirths_white <- ncbirths %>%
  filter(whitemom == "white")

mean(ncbirths_white$weight, na.rm = T)
```

    ## [1] 7.250462

### Exercise 3

We want to make an inference about what happens in the population, but
we only have a sample from the population, so this is a good situation
to use an inferential test.

### Exercise 4

First, i did one bootstrapped sample based on our sample of white NC
births, which ended up having a mean = 7.28.

``` r
birth_boot1 <- ncbirths_white %>%
  slice_sample(n = 1000, replace = T)

mean(birth_boot1$weight, na.rm = T)
```

    ## [1] 7.22885

Then, I ran 1000 bootstrapped samples, and plotted the distribution of
these sample means against the distribution of sample means under the
null hypothesis.

Then, I rat a t-test comparing these two distributions. The p-value for
this test is p \< .001, indicating that fewer than 0.1% of the
bootstrapped sample means under the null hypothesis were as extreme as
the observed mean of our sample.

From this, we can conclude that the average weight of white babies born
in NC in 2004 is most likely truly lower than the average weight of
white babies born in the US in 1995. I also calculated a 95% confidence
interval for our samples, which tells us we can be 95% confident that
the average weight in the population of white babies born in NC is
somewhere between 7.14 to 7.36 pounds.

``` r
birth_boot <- ncbirths_white %>%
  specify(response = weight) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
```

``` r
mean(birth_boot$stat, na.rm = T)
```

    ## [1] 7.24585

``` r
birth_boot <- birth_boot %>%
  mutate(null = stat + 0.179903)

ggplot(birth_boot)+
  geom_histogram(aes(x = null), fill = "grey80", color = "black", alpha = 0.5)+
  geom_histogram(aes(x = stat), fill = "blue3", color = "black", alpha = 0.5)+
  theme_classic()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lab-11_files/figure-gfm/compare-dists-1.png)<!-- -->

``` r
t.test(birth_boot$null, birth_boot$stat)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  birth_boot$null and birth_boot$stat
    ## t = 74.888, df = 1998, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1751917 0.1846143
    ## sample estimates:
    ## mean of x mean of y 
    ##  7.425753  7.245850

``` r
birth_boot %>%
  summarize(lower = quantile(stat, 0.025),
            upper = quantile(stat, 0.975))
```

    ## # A tibble: 1 × 2
    ##   lower upper
    ##   <dbl> <dbl>
    ## 1  7.14  7.35

### Exercise 5

I made side-by-side boxplots showing baby’s weight as a function of
whether or not the mother is a smoker. What this shows is that children
of smokers and non-smokers showed similar median weights at birth, but
weights were generally somewhat lower for children of smoking mothers,
and there were more outliers for children of non-smoking mothers.

``` r
ncbirths %>%
  filter(!is.na(habit)) %>%
ggplot(aes(x = habit, y = weight))+
  geom_boxplot()
```

![](lab-11_files/figure-gfm/boxes-1.png)<!-- -->

### Exercise 6

I made a filtered dataset to only have those cases when mothers reported
if they were smokers.

``` r
ncbirths_habitgiven <- ncbirths %>%
  filter(!is.na(habit))
```

### Exercise 7

While there is descriptively a difference between the mean weight for
children of smoking vs. non-smoking mothers, we want to know if this is
a statistically significant difference (e.g., likely to be actually
different between the two populations). Thus, we are going to see if we
can reject the null hypothesis:

H0: M(smokers) = M(non-smokers)

and find evidence for the alternative hypothesis:

H1: M(smokers) =/= M(non-smokers)

``` r
ncbirths_habitgiven %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight))
```

    ## # A tibble: 2 × 2
    ##   habit     mean_weight
    ##   <fct>           <dbl>
    ## 1 nonsmoker        7.14
    ## 2 smoker           6.83

### Exercise 8

Yes, because we want to infer from samples to a population.

### Exercise 9

``` r
smoker_boot <- ncbirths_habitgiven %>%
  specify(response = weight) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
```

``` r
mean(smoker_boot$stat, na.rm = T)
```

    ## [1] 7.103701

``` r
smoker_boot <- smoker_boot %>%
  mutate(nonsmoker_mean = stat + (7.144273 - 7.103702))

smoker_boot <- smoker_boot %>%
  mutate(smoker_mean = stat + (6.828730 - 7.103702))

ggplot(smoker_boot)+
  geom_histogram(aes(x = nonsmoker_mean), fill = "grey80", color = "black", alpha = 0.5)+
  geom_histogram(aes(x = smoker_mean), fill = "grey30", color = "black", alpha = 0.5)+
  theme_classic()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lab-11_files/figure-gfm/means-1.png)<!-- -->

``` r
t.test(smoker_boot$smoker_mean, smoker_boot$nonsmoker_mean)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  smoker_boot$smoker_mean and smoker_boot$nonsmoker_mean
    ## t = -147.99, df = 1998, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3197244 -0.3113616
    ## sample estimates:
    ## mean of x mean of y 
    ##  6.828729  7.144272
