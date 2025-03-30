# Eye-tracking paper
Zelionkaite

## Libraries

``` r
library("tidyverse")
library("rstatix")
library("emmeans")
library("dplyr")
```

``` r
load("eye_tracking_data.RData")
```

# self-reported data

## Questionnaires

``` r
self_reported%>%
 group_by(Group2) %>%
  summarise(M = mean(TAS_overall),
            SD = sd(TAS_overall))
```

    # A tibble: 3 × 3
      Group2     M    SD
      <chr>  <dbl> <dbl>
    1 Men     45.2  9.86
    2 NC      44.9 10.1 
    3 OC      46   12.9 

``` r
### QUESTIONNAIRES 

  Alexithymia <- aov(TAS_overall ~ Group2, data = self_reported)
summary(Alexithymia)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2     23   11.63   0.097  0.908
    Residuals   104  12475  119.95               

``` r
pairwise.t.test(self_reported$TAS_overall, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$TAS_overall and self_reported$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  Alexithymia_describe <- aov(TAS_Diff_Describ_Feel ~ Group2, data = self_reported)
summary(Alexithymia_describe)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2      7   3.498   0.172  0.842
    Residuals   104   2115  20.339               

``` r
pairwise.t.test(self_reported$TAS_Diff_Describ_Feel, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$TAS_Diff_Describ_Feel and self_reported$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  Alexithymia_identify <- aov(TAS_Diff_Identify_Feel ~ Group2, data = self_reported)
summary(Alexithymia_identify)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2  111.6   55.79    2.24  0.112
    Residuals   104 2590.6   24.91               

``` r
pairwise.t.test(self_reported$TAS_Diff_Identify_Feel, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$TAS_Diff_Identify_Feel and self_reported$Group2 

       Men  NC  
    NC 0.18 -   
    OC 0.18 0.82

    P value adjustment method: holm 

``` r
  Alexithymia_external <- aov(TAS_Exter_Orient_Think ~ Group2, data = self_reported)
summary(Alexithymia_external)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2   78.6   39.32   2.347  0.101
    Residuals   104 1742.1   16.75               

``` r
pairwise.t.test(self_reported$TAS_Exter_Orient_Think, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$TAS_Exter_Orient_Think and self_reported$Group2 

       Men   NC   
    NC 0.099 -    
    OC 0.448 0.448

    P value adjustment method: holm 

``` r
  PA <- aov(PA ~ Group2, data = self_reported)
summary(PA)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2     20    9.98   0.284  0.753
    Residuals   104   3655   35.14               

``` r
    pairwise.t.test(self_reported$PA, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$PA and self_reported$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  na <- aov(NA2 ~ Group2, data = self_reported)
summary(na)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2    216   108.2   2.142  0.123
    Residuals   104   5252    50.5               

``` r
  pairwise.t.test(self_reported$NA2, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$NA2 and self_reported$Group2 

       Men  NC  
    NC 0.26 -   
    OC 0.16 0.65

    P value adjustment method: holm 

``` r
  Age <- aov(Age ~ Group2, data = self_reported)
summary(Age)
```

                 Df Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  153.6   76.80    4.42 0.0144 *
    Residuals   104 1807.1   17.38                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
  pairwise.t.test(self_reported$Age, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Age and self_reported$Group2 

       Men   NC   
    NC 0.015 -    
    OC 0.526 0.071

    P value adjustment method: holm 

``` r
  BMI <- aov(BMI ~ Group2, data = self_reported)
summary(BMI)
```

                 Df Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  131.9   65.97   4.258 0.0167 *
    Residuals   104 1611.5   15.49                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
    pairwise.t.test(self_reported$BMI, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$BMI and self_reported$Group2 

       Men   NC   
    NC 0.034 -    
    OC 0.034 0.929

    P value adjustment method: holm 

``` r
  ANX <- aov(Anxiety ~ Group2, data = self_reported)
summary(ANX)
```

                 Df Sum Sq Mean Sq F value  Pr(>F)   
    Group2        2    145   72.52   5.651 0.00468 **
    Residuals   104   1335   12.83                   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
    pairwise.t.test(self_reported$Anxiety, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Anxiety and self_reported$Group2 

       Men    NC    
    NC 0.1737 -     
    OC 0.0033 0.1737

    P value adjustment method: holm 

``` r
  Education <- aov(Education_time ~ Group2, data = self_reported)
summary(Education)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2    8.5   4.240   1.343  0.266
    Residuals   104  328.3   3.157               

``` r
  pairwise.t.test(self_reported$Education_time, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Education_time and self_reported$Group2 

       Men  NC  
    NC 0.39 -   
    OC 0.42 0.82

    P value adjustment method: holm 

``` r
  Honesty <- aov(Honesty ~ Group2, data = self_reported)
summary(Honesty)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2   1.63  0.8151   1.679  0.192
    Residuals   104  50.49  0.4855               

``` r
  pairwise.t.test(self_reported$Honesty, self_reported$Group2, p.adjust.method = "holm")  
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Honesty and self_reported$Group2 

       Men  NC  
    NC 0.21 -   
    OC 0.68 0.68

    P value adjustment method: holm 

``` r
  Emotionality <- aov(Emotionality ~ Group2, data = self_reported)
summary(Emotionality)
```

                 Df Sum Sq Mean Sq F value  Pr(>F)    
    Group2        2   8.86   4.431   13.88 4.6e-06 ***
    Residuals   103  32.88   0.319                    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    1 stebėjimas panaikintas dėl netrūkstamumo

``` r
  pairwise.t.test(self_reported$Emotionality, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Emotionality and self_reported$Group2 

       Men     NC    
    NC 0.0022  -     
    OC 3.4e-06 0.0627

    P value adjustment method: holm 

``` r
  Extraversion <- aov(Extraversion ~ Group2, data = self_reported)
summary(Extraversion)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2   0.74  0.3722   0.696  0.501
    Residuals   103  55.06  0.5346               
    1 stebėjimas panaikintas dėl netrūkstamumo

``` r
  pairwise.t.test(self_reported$Extraversion, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Extraversion and self_reported$Group2 

       Men  NC  
    NC 0.95 -   
    OC 0.88 0.88

    P value adjustment method: holm 

``` r
  Agreeableness <- aov(Agreeableness ~ Group2, data = self_reported)
summary(Agreeableness)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2   0.63  0.3140   0.798  0.453
    Residuals   103  40.54  0.3936               
    1 stebėjimas panaikintas dėl netrūkstamumo

``` r
  pairwise.t.test(self_reported$Agreeableness, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Agreeableness and self_reported$Group2 

       Men  NC  
    NC 0.85 -   
    OC 0.85 0.64

    P value adjustment method: holm 

``` r
  Conscientiousness <- aov(Conscientiousness ~ Group2, data = self_reported)
summary(Conscientiousness)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2   0.12  0.0578   0.159  0.854
    Residuals   103  37.53  0.3644               
    1 stebėjimas panaikintas dėl netrūkstamumo

``` r
  pairwise.t.test(self_reported$Conscientiousness, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Conscientiousness and self_reported$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  Openness_to_experience <- aov(Openness_to_experience ~ Group2, data = self_reported)
summary(Openness_to_experience)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2   1.16  0.5825    1.67  0.193
    Residuals   103  35.92  0.3488               
    1 stebėjimas panaikintas dėl netrūkstamumo

``` r
  pairwise.t.test(self_reported$Openness_to_experience, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Openness_to_experience and self_reported$Group2 

       Men  NC  
    NC 0.95 -   
    OC 0.32 0.32

    P value adjustment method: holm 

``` r
### SEX STEROIDS    
    ## SEX
    
  testo <- aov(Testosterone ~ Group2, data = self_reported)
summary(testo)
```

                 Df Sum Sq Mean Sq F value Pr(>F)    
    Group2        2 712911  356456   129.2 <2e-16 ***
    Residuals   101 278690    2759                   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    3 stebėjimai panaikinti dėl netrūkstamumų

``` r
    pairwise.t.test(self_reported$Testosterone, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Testosterone and self_reported$Group2 

       Men    NC  
    NC <2e-16 -   
    OC <2e-16 0.17

    P value adjustment method: holm 

``` r
    ## WOMEN
#self_reported_women <- self_reported %>% filter(Group2 != "Men")

prog <-aov(Progesterone ~ Group2, data = self_reported_women)
summary(prog)
```

                Df Sum Sq Mean Sq F value Pr(>F)
    Group2       1   5554    5554   2.474  0.121
    Residuals   63 141438    2245               
    4 stebėjimai panaikinti dėl netrūkstamumų

``` r
  pairwise.t.test(self_reported_women$Progesterone, self_reported_women$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported_women$Progesterone and self_reported_women$Group2 

       NC  
    OC 0.12

    P value adjustment method: holm 

``` r
testo <- aov(Testosterone ~ Group2, data = self_reported_women)
summary(testo)
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    Group2       1   5280    5280   12.76 0.000672 ***
    Residuals   65  26892     414                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    2 stebėjimai panaikinti dėl netrūkstamumų

``` r
    pairwise.t.test(self_reported_women$Testosterone, self_reported_women$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported_women$Testosterone and self_reported_women$Group2 

       NC     
    OC 0.00067

    P value adjustment method: holm 

## Arousal/ valence

``` r
### NEUTRAL
  Arousal_neutral <- aov(Arousal_neutral ~ Group2, data = self_reported)
summary(Arousal_neutral)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2    595   297.6   1.437  0.242
    Residuals   104  21542   207.1               

``` r
pairwise.t.test(self_reported$Arousal_neutral, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Arousal_neutral and self_reported$Group2 

       Men  NC  
    NC 0.86 -   
    OC 0.37 0.37

    P value adjustment method: holm 

``` r
  Valence_neutral <- aov(Valence_neutral ~ Group2, data = self_reported)
summary(Valence_neutral)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2    104   51.79   0.539  0.585
    Residuals   104   9991   96.07               

``` r
pairwise.t.test(self_reported$Valence_neutral, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Valence_neutral and self_reported$Group2 

       Men  NC  
    NC 0.95 -   
    OC 0.95 0.95

    P value adjustment method: holm 

``` r
### NEGATIVE
  Arousal_negative <- aov(Arousal_negative ~ Group2, data = self_reported)
summary(Arousal_negative)
```

                 Df Sum Sq Mean Sq F value Pr(>F)
    Group2        2   1366   682.9   1.693  0.189
    Residuals   102  41133   403.3               
    2 stebėjimai panaikinti dėl netrūkstamumų

``` r
pairwise.t.test(self_reported$Arousal_negative, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Arousal_negative and self_reported$Group2 

       Men  NC  
    NC 0.26 -   
    OC 0.34 0.78

    P value adjustment method: holm 

``` r
  Valence_negative <- aov(Valence_negative ~ Group2, data = self_reported)
summary(Valence_negative)
```

                 Df Sum Sq Mean Sq F value Pr(>F)  
    Group2        2   1757   878.3   2.521 0.0854 .
    Residuals   102  35539   348.4                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    2 stebėjimai panaikinti dėl netrūkstamumų

``` r
pairwise.t.test(self_reported$Valence_negative, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Valence_negative and self_reported$Group2 

       Men   NC   
    NC 0.552 -    
    OC 0.092 0.226

    P value adjustment method: holm 

``` r
### EROTIC
  Arousal_erotic <- aov(Arousal_erotic ~ Group2, data = self_reported)
summary(Arousal_erotic)
```

                Df Sum Sq Mean Sq F value Pr(>F)
    Group2       2     50   24.93   0.091  0.913
    Residuals   98  26816  273.63               
    6 stebėjimai panaikinti dėl netrūkstamumų

``` r
pairwise.t.test(self_reported$Arousal_erotic, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Arousal_erotic and self_reported$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  Valence_erotic <- aov(Valence_erotic ~ Group2, data = self_reported)
summary(Valence_erotic)
```

                Df Sum Sq Mean Sq F value Pr(>F)  
    Group2       2   2028  1013.8   2.628 0.0773 .
    Residuals   98  37810   385.8                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    6 stebėjimai panaikinti dėl netrūkstamumų

``` r
pairwise.t.test(self_reported$Valence_erotic, self_reported$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  self_reported$Valence_erotic and self_reported$Group2 

       Men   NC   
    NC 0.349 -    
    OC 0.074 0.349

    P value adjustment method: holm 

## Emotional arousal

``` r
Emot_arousal%>%
  group_by(group, Condition) %>%
  summarise(M = mean(values),
            SD = sd(values))
```

    `summarise()` has grouped output by 'group'. You can override using the
    `.groups` argument.

    # A tibble: 6 × 4
    # Groups:   group [3]
      group Condition     M    SD
      <chr> <chr>     <dbl> <dbl>
    1 Men   After      34.6  22.8
    2 Men   Before     20.9  13.9
    3 NC    After      40.9  22.8
    4 NC    Before     35.1  16.7
    5 OC    After      35.7  21.1
    6 OC    Before     30.8  19.6

``` r
RM_ANOVA_arousal <- anova_test(Emot_arousal, dv = values, wid = id, within = Condition, between = group, effect.size = "pes")

RM_ANOVA_arousal$`Mauchly's Test for Sphericity`
```

    NULL

``` r
RM_ANOVA_arousal$`Sphericity Corrections`
```

    NULL

``` r
get_anova_table(RM_ANOVA_arousal)
```

    ANOVA Table (type III tests)

               Effect DFn DFd      F        p p<.05   pes
    1           group   2 104  3.665 0.029000     * 0.066
    2       Condition   1 104 14.640 0.000222     * 0.123
    3 group:Condition   2 104  1.767 0.176000       0.033

``` r
#posthoc
  RM_ANOVA_PostHoc_arousal <- Emot_arousal %>%
  emmeans_test(values ~ Condition, p.adjust.method = "holm")
print(RM_ANOVA_PostHoc_arousal)
```

    # A tibble: 1 × 9
      term      .y.    group1 group2    df statistic       p   p.adj p.adj.signif
    * <chr>     <chr>  <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
    1 Condition values After  Before   212      3.03 0.00272 0.00272 **          

``` r
  RM_ANOVA_PostHoc_arousal_grupe <- Emot_arousal %>%
  emmeans_test(values ~ group, p.adjust.method = "holm")
print(RM_ANOVA_PostHoc_arousal_grupe)
```

    # A tibble: 3 × 9
      term  .y.    group1 group2    df statistic       p   p.adj p.adj.signif
    * <chr> <chr>  <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
    1 group values Men    NC       211     -3.11 0.00213 0.00638 **          
    2 group values Men    OC       211     -1.64 0.103   0.206   ns          
    3 group values NC     OC       211      1.39 0.167   0.206   ns          

``` r
RM_ANOVA_PostHoc_arousal_interact <- Emot_arousal %>%
  group_by(Condition) %>%
  emmeans_test(values ~ group, p.adjust.method = "holm")
print(RM_ANOVA_PostHoc_arousal_interact)
```

    # A tibble: 6 × 10
      Condition term  .y.    group1 group2    df statistic       p   p.adj
    * <fct>     <chr> <chr>  <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl>
    1 After     group values Men    NC       208    -1.39  0.167   0.501  
    2 After     group values Men    OC       208    -0.244 0.807   0.807  
    3 After     group values NC     OC       208     1.10  0.274   0.548  
    4 Before    group values Men    NC       208    -3.10  0.00219 0.00656
    5 Before    group values Men    OC       208    -2.12  0.0353  0.0707 
    6 Before    group values NC     OC       208     0.902 0.368   0.368  
    # ℹ 1 more variable: p.adj.signif <chr>

# Gaze parameters

## Neutral

### Dwell time

``` r
Gaze_neutral%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(dwell_time),
            SD = sd(dwell_time))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 9 × 4
    # Groups:   Group2 [3]
      Group2 AOI2           M    SD
      <chr>  <chr>      <dbl> <dbl>
    1 Men    Background  40.2  5.08
    2 Men    Body        26.3  5.54
    3 Men    Face        33.5  7.08
    4 NC     Background  37.2  6.89
    5 NC     Body        24.9  4.63
    6 NC     Face        37.8  7.53
    7 OC     Background  38.7  5.14
    8 OC     Body        26.8  5.45
    9 OC     Face        34.5  6.87

``` r
Neut_RM_ANOVA_dwell_time <- anova_test(Gaze_neutral, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.849 0.000216     *
    2 Group2:AOI2 0.849 0.000216     *

``` r
Neut_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]   p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.869 1.74, 180.69 7.1e-24         * 0.882 1.76, 183.49 3.33e-24
    2 Group2:AOI2 0.869 3.47, 180.69 3.8e-02         * 0.882 3.53, 183.49 3.70e-02
      p[HF]<.05
    1         *
    2         *

``` r
get_anova_table(Neut_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd      F        p p<.05      pes
    1      Group2 2.00 104.00  0.004 9.96e-01       8.25e-05
    2        AOI2 1.74 180.69 82.416 7.10e-24     * 4.42e-01
    3 Group2:AOI2 3.47 180.69  2.721 3.80e-02     * 5.00e-02

``` r
#posthoc
Neut_RM_ANOVA_dwell_time_interact <- Gaze_neutral %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 9 × 10
      AOI2    term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
    * <fct>   <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
    1 Backgr… Grou… dwel… Men    NC       312     2.11  0.0356  0.107   ns          
    2 Backgr… Grou… dwel… Men    OC       312     1.01  0.312   0.601   ns          
    3 Backgr… Grou… dwel… NC     OC       312    -1.04  0.300   0.601   ns          
    4 Body    Grou… dwel… Men    NC       312     0.958 0.339   0.678   ns          
    5 Body    Grou… dwel… Men    OC       312    -0.322 0.748   0.748   ns          
    6 Body    Grou… dwel… NC     OC       312    -1.24  0.215   0.646   ns          
    7 Face    Grou… dwel… Men    NC       312    -3.07  0.00235 0.00704 **          
    8 Face    Grou… dwel… Men    OC       312    -0.690 0.491   0.491   ns          
    9 Face    Grou… dwel… NC     OC       312     2.28  0.0233  0.0467  *           

``` r
Neut_RM_ANOVA_dwell_time_zonos <- Gaze_neutral %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 3 × 9
      term  .y.        group1  group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>      <chr>   <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  dwell_time Backgr… Body     318     15.0  5.43e-39 1.63e-38 ****        
    2 AOI2  dwell_time Backgr… Face     318      4.12 4.78e- 5 4.78e- 5 ****        
    3 AOI2  dwell_time Body    Face     318    -10.9  8.82e-24 1.76e-23 ****        

``` r
Neut_RM_ANOVA_dwell_time_grupe <- Gaze_neutral %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       318 -9.69e-11  1.00     1 ns          
    2 Group2 dwell_time Men    OC       318  3.22e-10  1.00     1 ns          
    3 Group2 dwell_time NC     OC       318  4.11e-10  1.00     1 ns          

### Return probability

``` r
Gaze_neutral%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(return_prob),
            SD = sd(return_prob))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 9 × 4
    # Groups:   Group2 [3]
      Group2 AOI2           M     SD
      <chr>  <chr>      <dbl>  <dbl>
    1 Men    Background 0.379 0.0345
    2 Men    Body       0.344 0.0488
    3 Men    Face       0.278 0.0467
    4 NC     Background 0.367 0.0382
    5 NC     Body       0.327 0.0399
    6 NC     Face       0.306 0.0476
    7 OC     Background 0.374 0.0355
    8 OC     Body       0.345 0.0470
    9 OC     Face       0.281 0.0545

``` r
Neut_RM_ANOVA_grizimo_tik <- anova_test(Gaze_neutral, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_grizimo_tik$`Mauchly's Test for Sphericity`
```

           Effect     W     p p<.05
    1        AOI2 0.878 0.001     *
    2 Group2:AOI2 0.878 0.001     *

``` r
Neut_RM_ANOVA_grizimo_tik$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.892 1.78, 185.44 6.08e-21         * 0.906 1.81, 188.45 3.06e-21
    2 Group2:AOI2 0.892 3.57, 185.44 6.00e-02           0.906 3.62, 188.45 5.90e-02
      p[HF]<.05
    1         *
    2          

``` r
get_anova_table(Neut_RM_ANOVA_grizimo_tik)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd      F        p p<.05    pes
    1      Group2 2.00 104.00 -0.257 1.00e+00       -0.005
    2        AOI2 1.78 185.44 66.957 6.08e-21     *  0.392
    3 Group2:AOI2 3.57 185.44  2.382 6.00e-02        0.044

``` r
#posthoc
Neut_RM_ANOVA_grizimo_tik_interact <- Gaze_neutral %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_interact)
```

    # A tibble: 9 × 10
      AOI2     term  .y.   group1 group2    df statistic       p  p.adj p.adj.signif
    * <fct>    <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>       
    1 Backgro… Grou… retu… Men    NC       312     1.12  0.266   0.797  ns          
    2 Backgro… Grou… retu… Men    OC       312     0.445 0.657   1      ns          
    3 Backgro… Grou… retu… NC     OC       312    -0.637 0.524   1      ns          
    4 Body     Grou… retu… Men    NC       312     1.66  0.0985  0.233  ns          
    5 Body     Grou… retu… Men    OC       312    -0.173 0.863   0.863  ns          
    6 Body     Grou… retu… NC     OC       312    -1.77  0.0777  0.233  ns          
    7 Face     Grou… retu… Men    NC       312    -2.77  0.00590 0.0177 *           
    8 Face     Grou… retu… Men    OC       312    -0.272 0.786   0.786  ns          
    9 Face     Grou… retu… NC     OC       312     2.41  0.0167  0.0333 *           

``` r
Neut_RM_ANOVA_grizimo_tik_zonos <- Gaze_neutral %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_zonos)
```

    # A tibble: 3 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  return_prob Backg… Body     318      5.70 2.74e- 8 2.74e- 8 ****        
    2 AOI2  return_prob Backg… Face     318     13.9  8.07e-35 2.42e-34 ****        
    3 AOI2  return_prob Body   Face     318      8.25 4.38e-15 8.76e-15 ****        

``` r
Neut_RM_ANOVA_grizimo_tik_grupe <- Gaze_neutral %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df     statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>         <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       318 0.00000000102  1.00     1 ns          
    2 Group2 return_prob Men    OC       318 0.00000000856  1.00     1 ns          
    3 Group2 return_prob NC     OC       318 0.00000000746  1.00     1 ns          

### Looking probability

``` r
Gaze_neutral%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(looking_prob),
            SD = sd(looking_prob))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 9 × 4
    # Groups:   Group2 [3]
      Group2 AOI2            M     SD
      <chr>  <chr>       <dbl>  <dbl>
    1 Men    Background  0.550 0.0700
    2 Men    Body        1.64  0.422 
    3 Men    Face       11.1   4.56  
    4 NC     Background  0.505 0.0994
    5 NC     Body        1.63  0.374 
    6 NC     Face       12.2   4.63  
    7 OC     Background  0.528 0.0764
    8 OC     Body        1.67  0.363 
    9 OC     Face       10.5   4.33  

``` r
Neut_RM_ANOVA_looking_prob <- anova_test(Gaze_neutral, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.022 1.17e-85     *
    2 Group2:AOI2 0.022 1.17e-85     *

``` r
Neut_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.506 1.01, 105.18 9.83e-43         * 0.506 1.01, 105.21 9.54e-43
    2 Group2:AOI2 0.506 2.02, 105.18 2.85e-01           0.506 2.02, 105.21 2.85e-01
      p[HF]<.05
    1         *
    2          

``` r
get_anova_table(Neut_RM_ANOVA_looking_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd       F        p p<.05   pes
    1      Group2 2.00 104.00   1.282 2.82e-01       0.024
    2        AOI2 1.01 105.18 520.749 9.83e-43     * 0.834
    3 Group2:AOI2 2.02 105.18   1.271 2.85e-01       0.024

``` r
#posthoc
Neut_RM_ANOVA_looking_prob_interact <- Gaze_neutral %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 9 × 10
      AOI2     term  .y.   group1 group2    df statistic       p  p.adj p.adj.signif
    * <fct>    <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>       
    1 Backgro… Grou… look… Men    NC       312    0.0731 0.942   1      ns          
    2 Backgro… Grou… look… Men    OC       312    0.0348 0.972   1      ns          
    3 Backgro… Grou… look… NC     OC       312   -0.0362 0.971   1      ns          
    4 Body     Grou… look… Men    NC       312    0.0188 0.985   1      ns          
    5 Body     Grou… look… Men    OC       312   -0.0475 0.962   1      ns          
    6 Body     Grou… look… NC     OC       312   -0.0650 0.948   1      ns          
    7 Face     Grou… look… Men    NC       312   -1.90   0.0582  0.116  ns          
    8 Face     Grou… look… Men    OC       312    0.861  0.390   0.390  ns          
    9 Face     Grou… look… NC     OC       312    2.68   0.00766 0.0230 *           

``` r
Neut_RM_ANOVA_looking_prob_zonos <- Gaze_neutral %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  looking_pr… Backg… Body     318     -3.11 2.04e- 3 2.04e- 3 **          
    2 AOI2  looking_pr… Backg… Face     318    -30.0  1.11e-94 3.33e-94 ****        
    3 AOI2  looking_pr… Body   Face     318    -26.9  8.00e-84 1.60e-83 ****        

``` r
Neut_RM_ANOVA_looking_prob_grupe <- Gaze_neutral %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       318    -0.496 0.620     1 ns          
    2 Group2 looking_prob Men    OC       318     0.232 0.816     1 ns          
    3 Group2 looking_prob NC     OC       318     0.708 0.479     1 ns          

## Negative

### Dwell time

``` r
Gaze_negative%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(dwell_time),
            SD = sd(dwell_time))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 9 × 4
    # Groups:   Group2 [3]
      Group2 AOI2            M    SD
      <chr>  <chr>       <dbl> <dbl>
    1 Men    Background   11.6  2.84
    2 Men    Intact       10.9  3.14
    3 Men    Mutilations  77.5  5.36
    4 NC     Background   10.7  2.81
    5 NC     Intact       10.5  2.98
    6 NC     Mutilations  78.8  5.01
    7 OC     Background   11.6  3.02
    8 OC     Intact       11.8  4.33
    9 OC     Mutilations  76.6  6.05

``` r
Neig_RM_ANOVA_dwell_time <- anova_test(Gaze_negative, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.377 3.86e-22     *
    2 Group2:AOI2 0.377 3.86e-22     *

``` r
Neig_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]     p[GG] p[GG]<.05  HFe       DF[HF]
    1        AOI2 0.616 1.23, 125.67 1.07e-113         * 0.62 1.24, 126.43
    2 Group2:AOI2 0.616 2.46, 125.67  2.76e-01           0.62 2.48, 126.43
          p[HF] p[HF]<.05
    1 2.28e-114         *
    2  2.76e-01          

``` r
Neig_RM_ANOVA_dwell_time$`emmeans`
```

    NULL

``` r
get_anova_table(Neig_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd        F         p p<.05      pes
    1      Group2 2.00 102.00    0.001  9.99e-01       2.71e-05
    2        AOI2 1.23 125.67 6106.657 1.07e-113     * 9.84e-01
    3 Group2:AOI2 2.46 125.67    1.306  2.76e-01       2.50e-02

``` r
#posthoc
Neig_RM_ANOVA_dwell_time_interact <- Gaze_negative %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 9 × 10
      AOI2      term  .y.   group1 group2    df statistic      p  p.adj p.adj.signif
    * <fct>     <chr> <chr> <chr>  <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>       
    1 Backgrou… Grou… dwel… Men    NC       306   0.923   0.357  1      ns          
    2 Backgrou… Grou… dwel… Men    OC       306   0.00297 0.998  1      ns          
    3 Backgrou… Grou… dwel… NC     OC       306  -0.873   0.383  1      ns          
    4 Intact    Grou… dwel… Men    NC       306   0.481   0.631  0.764  ns          
    5 Intact    Grou… dwel… Men    OC       306  -0.876   0.382  0.764  ns          
    6 Intact    Grou… dwel… NC     OC       306  -1.32    0.187  0.561  ns          
    7 Mutilati… Grou… dwel… Men    NC       306  -1.40    0.161  0.322  ns          
    8 Mutilati… Grou… dwel… Men    OC       306   0.873   0.383  0.383  ns          
    9 Mutilati… Grou… dwel… NC     OC       306   2.20    0.0289 0.0866 ns          

``` r
Neig_RM_ANOVA_dwell_time_zonos <- Gaze_negative %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  dwell_ti… Backg… Intact   312     0.527 5.99e-  1 5.99e-  1 ns          
    2 AOI2  dwell_ti… Backg… Mutil…   312  -117.    5.76e-260 1.15e-259 ****        
    3 AOI2  dwell_ti… Intact Mutil…   312  -118.    1.47e-260 4.40e-260 ****        

``` r
Neig_RM_ANOVA_dwell_time_grupe <- Gaze_negative %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       312  4.69e-11  1.00     1 ns          
    2 Group2 dwell_time Men    OC       312  9.01e-11  1.00     1 ns          
    3 Group2 dwell_time NC     OC       312  4.44e-11  1.00     1 ns          

### Return probability

``` r
Gaze_negative%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(return_prob),
            SD = sd(return_prob))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 9 × 4
    # Groups:   Group2 [3]
      Group2 AOI2            M     SD
      <chr>  <chr>       <dbl>  <dbl>
    1 Men    Background  0.205 0.0435
    2 Men    Intact      0.166 0.0429
    3 Men    Mutilations 0.629 0.0546
    4 NC     Background  0.179 0.0398
    5 NC     Intact      0.161 0.0517
    6 NC     Mutilations 0.661 0.0675
    7 OC     Background  0.187 0.0423
    8 OC     Intact      0.165 0.0575
    9 OC     Mutilations 0.648 0.0668

``` r
Neig_RM_ANOVA_return_prob <- anova_test(Gaze_negative, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_return_prob$`Mauchly's Test for Sphericity`
```

           Effect     W       p p<.05
    1        AOI2 0.781 3.9e-06     *
    2 Group2:AOI2 0.781 3.9e-06     *

``` r
Neig_RM_ANOVA_return_prob$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]     p[GG] p[GG]<.05   HFe       DF[HF]
    1        AOI2 0.821 1.64, 167.41 1.71e-108         * 0.832 1.66, 169.78
    2 Group2:AOI2 0.821 3.28, 167.41  1.18e-01           0.832 3.33, 169.78
          p[HF] p[HF]<.05
    1 5.46e-110         *
    2  1.17e-01          

``` r
Neig_RM_ANOVA_return_prob$`emmeans`
```

    NULL

``` r
get_anova_table(Neig_RM_ANOVA_return_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd        F         p p<.05      pes
    1      Group2 2.00 102.00    0.003  9.97e-01       6.35e-05
    2        AOI2 1.64 167.41 1853.702 1.71e-108     * 9.48e-01
    3 Group2:AOI2 3.28 167.41    1.949  1.18e-01       3.70e-02

``` r
#posthoc
Neig_RM_ANOVA_return_prob_interact <- Gaze_negative %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_interact)
```

    # A tibble: 9 × 10
      AOI2      term  .y.   group1 group2    df statistic      p  p.adj p.adj.signif
    * <fct>     <chr> <chr> <chr>  <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>       
    1 Backgrou… Grou… retu… Men    NC       306     2.13  0.0342 0.103  ns          
    2 Backgrou… Grou… retu… Men    OC       306     1.37  0.171  0.341  ns          
    3 Backgrou… Grou… retu… NC     OC       306    -0.663 0.508  0.508  ns          
    4 Intact    Grou… retu… Men    NC       306     0.454 0.650  1      ns          
    5 Intact    Grou… retu… Men    OC       306     0.109 0.913  1      ns          
    6 Intact    Grou… retu… NC     OC       306    -0.324 0.746  1      ns          
    7 Mutilati… Grou… retu… Men    NC       306    -2.58  0.0103 0.0309 *           
    8 Mutilati… Grou… retu… Men    OC       306    -1.48  0.139  0.279  ns          
    9 Mutilati… Grou… retu… NC     OC       306     0.987 0.325  0.325  ns          

``` r
Neig_RM_ANOVA_return_prob_zonos <- Gaze_negative %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  return_p… Backg… Intact   312      3.66 2.98e-  4 2.98e-  4 ***         
    2 AOI2  return_p… Backg… Mutil…   312    -62.2  5.57e-178 1.11e-177 ****        
    3 AOI2  return_p… Intact Mutil…   312    -65.9  3.57e-185 1.07e-184 ****        

``` r
Neig_RM_ANOVA_return_prob_grupe <- Gaze_negative %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       312 -3.34e-10  1.00     1 ns          
    2 Group2 return_prob Men    OC       312 -1.24e-10  1.00     1 ns          
    3 Group2 return_prob NC     OC       312  1.94e-10  1.00     1 ns          

### Looking probability

``` r
Gaze_negative%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(looking_prob),
            SD = sd(looking_prob))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 9 × 4
    # Groups:   Group2 [3]
      Group2 AOI2             M     SD
      <chr>  <chr>        <dbl>  <dbl>
    1 Men    Background   0.226 0.0619
    2 Men    Intact       3.51  2.04  
    3 Men    Mutilations 14.0   3.35  
    4 NC     Background   0.206 0.0565
    5 NC     Intact       3.55  1.41  
    6 NC     Mutilations 15.0   2.92  
    7 OC     Background   0.225 0.0632
    8 OC     Intact       3.90  2.81  
    9 OC     Mutilations 15.0   4.09  

``` r
Neig_RM_ANOVA_looking_prob <- anova_test(Gaze_negative, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.589 2.44e-12     *
    2 Group2:AOI2 0.589 2.44e-12     *

``` r
Neig_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.709 1.42, 144.57 6.58e-78         * 0.716 1.43, 146.02 1.17e-78
    2 Group2:AOI2 0.709 2.83, 144.57 6.16e-01           0.716 2.86, 146.02 6.18e-01
      p[HF]<.05
    1         *
    2          

``` r
Neig_RM_ANOVA_looking_prob$`emmeans`
```

    NULL

``` r
get_anova_table(Neig_RM_ANOVA_looking_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd        F        p p<.05   pes
    1      Group2 2.00 102.00    1.041 3.57e-01       0.020
    2        AOI2 1.42 144.57 1066.123 6.58e-78     * 0.913
    3 Group2:AOI2 2.83 144.57    0.585 6.16e-01       0.011

``` r
#posthoc
Neig_RM_ANOVA_looking_prob_interact <- Gaze_negative %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 9 × 10
      AOI2       term  .y.   group1 group2    df statistic      p p.adj p.adj.signif
    * <fct>      <chr> <chr> <chr>  <chr>  <dbl>     <dbl>  <dbl> <dbl> <chr>       
    1 Background Grou… look… Men    NC       306   0.0363  0.971  1     ns          
    2 Background Grou… look… Men    OC       306   0.00198 0.998  1     ns          
    3 Background Grou… look… NC     OC       306  -0.0325  0.974  1     ns          
    4 Intact     Grou… look… Men    NC       306  -0.0822  0.935  1     ns          
    5 Intact     Grou… look… Men    OC       306  -0.689   0.491  1     ns          
    6 Intact     Grou… look… NC     OC       306  -0.603   0.547  1     ns          
    7 Mutilatio… Grou… look… Men    NC       306  -1.69    0.0911 0.273 ns          
    8 Mutilatio… Grou… look… Men    OC       306  -1.66    0.0973 0.273 ns          
    9 Mutilatio… Grou… look… NC     OC       306  -0.0340  0.973  0.973 ns          

``` r
Neig_RM_ANOVA_looking_prob_zonos <- Gaze_negative %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  looking_… Backg… Intact   312     -10.6 1.16e- 22 1.16e- 22 ****        
    2 AOI2  looking_… Backg… Mutil…   312     -44.7 8.96e-138 2.69e-137 ****        
    3 AOI2  looking_… Intact Mutil…   312     -34.1 2.46e-107 4.92e-107 ****        

``` r
Neig_RM_ANOVA_looking_prob_grupe <- Gaze_negative %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       312    -0.356 0.722     1 ns          
    2 Group2 looking_prob Men    OC       312    -0.481 0.631     1 ns          
    3 Group2 looking_prob NC     OC       312    -0.137 0.891     1 ns          

## Erotic

### Dwell time

``` r
Gaze_erotic%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(dwell_time),
            SD = sd(dwell_time))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 12 × 4
    # Groups:   Group2 [3]
       Group2 AOI2           M    SD
       <chr>  <chr>      <dbl> <dbl>
     1 Men    Background  3.37  2.25
     2 Men    Body       35.5   5.08
     3 Men    Erogenous  12.5   3.98
     4 Men    Face       48.6   6.06
     5 NC     Background  3.43  2.55
     6 NC     Body       33.2   4.33
     7 NC     Erogenous   9.69  2.61
     8 NC     Face       53.7   6.48
     9 OC     Background  4.35  3.51
    10 OC     Body       35.4   5.40
    11 OC     Erogenous  10.5   3.54
    12 OC     Face       49.7   5.23

``` r
Teig_RM_ANOVA_dwell_time <- anova_test(Gaze_erotic, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.381 1.41e-18     *
    2 Group2:AOI2 0.381 1.41e-18     *

``` r
Teig_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]     p[GG] p[GG]<.05   HFe       DF[HF]
    1        AOI2 0.612 1.84, 179.88 2.76e-116         * 0.623 1.87, 183.14
    2 Group2:AOI2 0.612 3.67, 179.88  2.00e-03         * 0.623 3.74, 183.14
          p[HF] p[HF]<.05
    1 2.41e-118         *
    2  1.00e-03         *

``` r
Teig_RM_ANOVA_dwell_time$`emmeans`
```

    NULL

``` r
get_anova_table(Teig_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd            F         p p<.05       pes
    1      Group2 2.00  98.00   -0.0000976  1.00e+00       -1.99e-06
    2        AOI2 1.84 179.88 1781.6090000 2.76e-116     *  9.48e-01
    3 Group2:AOI2 3.67 179.88    4.7590000  2.00e-03     *  8.90e-02

``` r
#posthoc
Teig_RM_ANOVA_dwell_time_interact <- Gaze_erotic %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… dwel… Men    NC       392   -0.0580 9.54e-1 1   e+0 ns          
     2 Backg… Grou… dwel… Men    OC       392   -0.894  3.72e-1 1   e+0 ns          
     3 Backg… Grou… dwel… NC     OC       392   -0.844  3.99e-1 1   e+0 ns          
     4 Body   Grou… dwel… Men    NC       392    2.15   3.20e-2 9.60e-2 ns          
     5 Body   Grou… dwel… Men    OC       392    0.105  9.17e-1 9.17e-1 ns          
     6 Body   Grou… dwel… NC     OC       392   -2.01   4.48e-2 9.60e-2 ns          
     7 Eroge… Grou… dwel… Men    NC       392    2.58   1.03e-2 3.09e-2 *           
     8 Eroge… Grou… dwel… Men    OC       392    1.75   8.10e-2 1.62e-1 ns          
     9 Eroge… Grou… dwel… NC     OC       392   -0.777  4.38e-1 4.38e-1 ns          
    10 Face   Grou… dwel… Men    NC       392   -4.67   4.10e-6 1.23e-5 ****        
    11 Face   Grou… dwel… Men    OC       392   -0.960  3.38e-1 3.38e-1 ns          
    12 Face   Grou… dwel… NC     OC       392    3.63   3.17e-4 6.33e-4 ***         

``` r
Teig_RM_ANOVA_dwell_time_zonos <- Gaze_erotic %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 6 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  dwell_ti… Backg… Body     400     -47.6 6.19e-167 2.48e-166 ****        
    2 AOI2  dwell_ti… Backg… Eroge…   400     -11.1 5.51e- 25 5.51e- 25 ****        
    3 AOI2  dwell_ti… Backg… Face     400     -72.3 1.17e-231 7.02e-231 ****        
    4 AOI2  dwell_ti… Body   Eroge…   400      36.6 1.22e-129 3.67e-129 ****        
    5 AOI2  dwell_ti… Body   Face     400     -24.6 3.67e- 82 7.34e- 82 ****        
    6 AOI2  dwell_ti… Eroge… Face     400     -61.2 3.38e-205 1.69e-204 ****        

``` r
Teig_RM_ANOVA_dwell_time_grupe <- Gaze_erotic %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       401 -9.90e-12  1.00     1 ns          
    2 Group2 dwell_time Men    OC       401  3.10e-11  1.00     1 ns          
    3 Group2 dwell_time NC     OC       401  4.10e-11  1.00     1 ns          

### Return probability

``` r
Gaze_erotic%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(return_prob),
            SD = sd(return_prob))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 12 × 4
    # Groups:   Group2 [3]
       Group2 AOI2            M     SD
       <chr>  <chr>       <dbl>  <dbl>
     1 Men    Background 0.0245 0.0219
     2 Men    Body       0.420  0.0632
     3 Men    Erogenous  0.131  0.0424
     4 Men    Face       0.425  0.0711
     5 NC     Background 0.0272 0.0225
     6 NC     Body       0.391  0.0544
     7 NC     Erogenous  0.104  0.0346
     8 NC     Face       0.478  0.0734
     9 OC     Background 0.0300 0.0239
    10 OC     Body       0.409  0.0647
    11 OC     Erogenous  0.113  0.0444
    12 OC     Face       0.448  0.0630

``` r
Teig_RM_ANOVA_return_prob <- anova_test(Gaze_erotic, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_return_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.229 5.32e-29     *
    2 Group2:AOI2 0.229 5.32e-29     *

``` r
Teig_RM_ANOVA_return_prob$`Sphericity Corrections`
```

           Effect  GGe      DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.56 1.68, 164.6 9.34e-95         * 0.569 1.71, 167.15 3.56e-96
    2 Group2:AOI2 0.56 3.36, 164.6 1.10e-02         * 0.569 3.41, 167.15 1.10e-02
      p[HF]<.05
    1         *
    2         *

``` r
Teig_RM_ANOVA_return_prob$`emmeans`
```

    NULL

``` r
get_anova_table(Teig_RM_ANOVA_return_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn   DFd        F        p p<.05   pes
    1      Group2 2.00  98.0    1.312 2.74e-01       0.026
    2        AOI2 1.68 164.6 1249.253 9.34e-95     * 0.927
    3 Group2:AOI2 3.36 164.6    3.614 1.10e-02     * 0.069

``` r
#posthoc
Teig_RM_ANOVA_return_prob_interact <- Gaze_erotic %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… retu… Men    NC       392    -0.213 8.31e-1 1   e+0 ns          
     2 Backg… Grou… retu… Men    OC       392    -0.427 6.69e-1 1   e+0 ns          
     3 Backg… Grou… retu… NC     OC       392    -0.220 8.26e-1 1   e+0 ns          
     4 Body   Grou… retu… Men    NC       392     2.31  2.12e-2 6.35e-2 ns          
     5 Body   Grou… retu… Men    OC       392     0.868 3.86e-1 3.86e-1 ns          
     6 Body   Grou… retu… NC     OC       392    -1.40  1.61e-1 3.22e-1 ns          
     7 Eroge… Grou… retu… Men    NC       392     2.18  3.01e-2 9.04e-2 ns          
     8 Eroge… Grou… retu… Men    OC       392     1.41  1.61e-1 3.21e-1 ns          
     9 Eroge… Grou… retu… NC     OC       392    -0.727 4.68e-1 4.68e-1 ns          
    10 Face   Grou… retu… Men    NC       392    -4.28  2.38e-5 7.14e-5 ****        
    11 Face   Grou… retu… Men    OC       392    -1.85  6.57e-2 6.57e-2 ns          
    12 Face   Grou… retu… NC     OC       392     2.35  1.92e-2 3.83e-2 *           

``` r
Teig_RM_ANOVA_return_prob_zonos <- Gaze_erotic %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_zonos)
```

    # A tibble: 6 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  return_p… Backg… Body     400    -50.8  1.30e-176 6.48e-176 ****        
    2 AOI2  return_p… Backg… Eroge…   400    -11.9  4.70e- 28 9.39e- 28 ****        
    3 AOI2  return_p… Backg… Face     400    -56.8  1.55e-193 9.28e-193 ****        
    4 AOI2  return_p… Body   Eroge…   400     39.0  3.38e-138 1.01e-137 ****        
    5 AOI2  return_p… Body   Face     400     -5.96 5.51e-  9 5.51e-  9 ****        
    6 AOI2  return_p… Eroge… Face     400    -44.9  2.46e-158 9.86e-158 ****        

``` r
Teig_RM_ANOVA_return_prob_grupe <- Gaze_erotic %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       401  -2.23e-9  1.00     1 ns          
    2 Group2 return_prob Men    OC       401  -4.24e-9  1.00     1 ns          
    3 Group2 return_prob NC     OC       401  -2.07e-9  1.00     1 ns          

### Looking probability

``` r
Gaze_erotic%>%
  group_by(Group2, AOI2) %>%
  summarise(M = mean(looking_prob),
            SD = sd(looking_prob))
```

    `summarise()` has grouped output by 'Group2'. You can override using the
    `.groups` argument.

    # A tibble: 12 × 4
    # Groups:   Group2 [3]
       Group2 AOI2             M      SD
       <chr>  <chr>        <dbl>   <dbl>
     1 Men    Background  0.0864  0.0599
     2 Men    Body        3.18    0.635 
     3 Men    Erogenous  43.6    20.6   
     4 Men    Face        8.68    1.62  
     5 NC     Background  0.0852  0.0659
     6 NC     Body        2.99    0.495 
     7 NC     Erogenous  34.6    15.4   
     8 NC     Face       10.1     1.90  
     9 OC     Background  0.109   0.101 
    10 OC     Body        3.11    0.685 
    11 OC     Erogenous  37.5    20.4   
    12 OC     Face        8.70    1.72  

``` r
Teig_RM_ANOVA_looking_prob <- anova_test(Gaze_erotic, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect        W         p p<.05
    1        AOI2 0.000115 4.58e-187     *
    2 Group2:AOI2 0.000115 4.58e-187     *

``` r
Teig_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect  GGe      DF[GG]    p[GG] p[GG]<.05  HFe      DF[HF]    p[HF]
    1        AOI2 0.34 1.02, 99.91 1.52e-34         * 0.34 1.02, 99.96 1.46e-34
    2 Group2:AOI2 0.34 2.04, 99.91 1.13e-01           0.34 2.04, 99.96 1.13e-01
      p[HF]<.05
    1         *
    2          

``` r
Teig_RM_ANOVA_looking_prob$`emmeans`
```

    NULL

``` r
get_anova_table(Teig_RM_ANOVA_looking_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn   DFd       F        p p<.05   pes
    1      Group2 2.00 98.00   1.612 2.05e-01       0.032
    2        AOI2 1.02 99.91 346.382 1.52e-34     * 0.779
    3 Group2:AOI2 2.04 99.91   2.222 1.13e-01       0.043

``` r
#posthoc
Teig_RM_ANOVA_looking_prob_interact <- Gaze_erotic %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… look… Men    NC       392  0.000496 1.00e+0 1   e+0 ns          
     2 Backg… Grou… look… Men    OC       392 -0.00953  9.92e-1 1   e+0 ns          
     3 Backg… Grou… look… NC     OC       392 -0.0101   9.92e-1 1   e+0 ns          
     4 Body   Grou… look… Men    NC       392  0.0816   9.35e-1 1   e+0 ns          
     5 Body   Grou… look… Men    OC       392  0.0305   9.76e-1 1   e+0 ns          
     6 Body   Grou… look… NC     OC       392 -0.0496   9.60e-1 1   e+0 ns          
     7 Eroge… Grou… look… Men    NC       392  3.93     9.96e-5 2.99e-4 ***         
     8 Eroge… Grou… look… Men    OC       392  2.62     9.01e-3 1.80e-2 *           
     9 Eroge… Grou… look… NC     OC       392 -1.23     2.20e-1 2.20e-1 ns          
    10 Face   Grou… look… Men    NC       392 -0.616    5.39e-1 1   e+0 ns          
    11 Face   Grou… look… Men    OC       392 -0.00947  9.92e-1 1   e+0 ns          
    12 Face   Grou… look… NC     OC       392  0.597    5.51e-1 1   e+0 ns          

``` r
Teig_RM_ANOVA_looking_prob_zonos <- Gaze_erotic %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 6 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  looking_pr… Backg… Body     400     -2.22 2.70e- 2 2.70e- 2 *           
    2 AOI2  looking_pr… Backg… Eroge…   400    -28.5  3.19e-98 1.92e-97 ****        
    3 AOI2  looking_pr… Backg… Face     400     -6.72 6.20e-11 1.86e-10 ****        
    4 AOI2  looking_pr… Body   Eroge…   400    -26.3  5.00e-89 2.50e-88 ****        
    5 AOI2  looking_pr… Body   Face     400     -4.50 8.82e- 6 1.76e- 5 ****        
    6 AOI2  looking_pr… Eroge… Face     400     21.7  8.81e-70 3.52e-69 ****        

``` r
Teig_RM_ANOVA_looking_prob_grupe <- Gaze_erotic %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       401     0.894 0.372     1 ns          
    2 Group2 looking_prob Men    OC       401     0.694 0.488     1 ns          
    3 Group2 looking_prob NC     OC       401    -0.182 0.856     1 ns          

# Pupillary parameters

## Neutral

``` r
##### NEUTRALUS
  LT_amplit_neurtral <- aov(LR_amplit_neutral ~ Group2, data = Pupil_neutral)
summary(LT_amplit_neurtral)
```

                 Df Sum Sq Mean Sq F value   Pr(>F)    
    Group2        2  0.743  0.3717   9.631 0.000145 ***
    Residuals   104  4.014  0.0386                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral$LR_amplit_neutral, Pupil_neutral$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral$LR_amplit_neutral and Pupil_neutral$Group2 

       Men     NC     
    NC 0.00042 -      
    OC 0.00115 0.75615

    P value adjustment method: holm 

``` r
  LT_latency_neurtral <- aov(LR_latency_neutral ~ Group2, data = Pupil_neutral)
summary(LT_latency_neurtral)
```

                 Df  Sum Sq Mean Sq F value Pr(>F)
    Group2        2   61355   30678   0.433   0.65
    Residuals   104 7365222   70819               

``` r
pairwise.t.test(Pupil_neutral$LR_latency_neutral, Pupil_neutral$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral$LR_latency_neutral and Pupil_neutral$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  Slope_neutral <- aov(Slope_neutral ~ Group2, data = Pupil_neutral)
summary(Slope_neutral)
```

                 Df    Sum Sq   Mean Sq F value Pr(>F)  
    Group2        2 4.320e-08 2.161e-08   3.618 0.0303 *
    Residuals   104 6.212e-07 5.973e-09                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral$Slope_neutral, Pupil_neutral$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral$Slope_neutral and Pupil_neutral$Group2 

       Men  NC  
    NC 0.03 -   
    OC 0.16 0.42

    P value adjustment method: holm 

``` r
  AUC_neutral <- aov(AUC_neutral ~ Group2, data = Pupil_neutral)
summary(AUC_neutral)
```

                 Df   Sum Sq Mean Sq F value  Pr(>F)   
    Group2        2  1230024  615012   5.301 0.00642 **
    Residuals   104 12066124  116020                   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral$AUC_neutral, Pupil_neutral$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral$AUC_neutral and Pupil_neutral$Group2 

       Men   NC   
    NC 0.016 -    
    OC 0.016 0.980

    P value adjustment method: holm 

``` r
  Mean_diameter_neutral <- aov(Mean_diameter_neutral ~ Group2, data = Pupil_neutral)
summary(Mean_diameter_neutral)
```

                 Df Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  0.355 0.17738   3.751 0.0267 *
    Residuals   104  4.918 0.04729                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral$Mean_diameter_neutral, Pupil_neutral$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral$Mean_diameter_neutral and Pupil_neutral$Group2 

       Men   NC   
    NC 0.022 -    
    OC 0.301 0.301

    P value adjustment method: holm 

## Negative

``` r
  LR_amplit_negative <- aov(LR_amplit_negative ~ Group2, data = Pupil_negative)
summary(LR_amplit_negative)
```

                 Df Sum Sq Mean Sq F value  Pr(>F)    
    Group2        2  0.993  0.4963   10.39 7.8e-05 ***
    Residuals   102  4.871  0.0478                    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative$LR_amplit_negative, Pupil_negative$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative$LR_amplit_negative and Pupil_negative$Group2 

       Men     NC    
    NC 8.3e-05 -     
    OC 0.0044  0.2886

    P value adjustment method: holm 

``` r
  LR_latency_negative <- aov(LR_latency_negative ~ Group2, data = Pupil_negative)
summary(LR_latency_negative)
```

                 Df  Sum Sq Mean Sq F value Pr(>F)
    Group2        2  313235  156617   2.284  0.107
    Residuals   102 6994079   68569               

``` r
pairwise.t.test(Pupil_negative$LR_latency_negative, Pupil_negative$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative$LR_latency_negative and Pupil_negative$Group2 

       Men  NC  
    NC 0.86 -   
    OC 0.16 0.16

    P value adjustment method: holm 

``` r
  Slope_negative <- aov(Slope_negative ~ Group2, data = Pupil_negative)
summary(Slope_negative)
```

                 Df    Sum Sq   Mean Sq F value Pr(>F)
    Group2        2 4.610e-08 2.307e-08   2.021  0.138
    Residuals   102 1.164e-06 1.141e-08               

``` r
pairwise.t.test(Pupil_negative$Slope_negative, Pupil_negative$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative$Slope_negative and Pupil_negative$Group2 

       Men  NC  
    NC 0.15 -   
    OC 0.50 0.45

    P value adjustment method: holm 

``` r
  AUC_negative <- aov(AUC_negative ~ Group2, data = Pupil_negative)
summary(AUC_negative)
```

                 Df   Sum Sq Mean Sq F value Pr(>F)
    Group2        2   976464  488232   2.311  0.104
    Residuals   102 21552416  211298               

``` r
pairwise.t.test(Pupil_negative$AUC_negative, Pupil_negative$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative$AUC_negative and Pupil_negative$Group2 

       Men  NC  
    NC 0.14 -   
    OC 0.77 0.21

    P value adjustment method: holm 

``` r
Mean_diameter_negative <- aov(Mean_diameter_negative ~ Group2, data = Pupil_negative)
summary(Mean_diameter_negative)
```

                 Df Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  0.352 0.17609   3.686 0.0285 *
    Residuals   102  4.873 0.04777                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative$Mean_diameter_negative, Pupil_negative$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative$Mean_diameter_negative and Pupil_negative$Group2 

       Men   NC   
    NC 0.023 -    
    OC 0.370 0.370

    P value adjustment method: holm 

## Erotic

``` r
  LR_amplit_ero <- aov(LR_amplit_erotic ~ Group2, data = Pupil_erotic)
summary(LR_amplit_ero)
```

                Df Sum Sq Mean Sq F value  Pr(>F)   
    Group2       2  0.796  0.3982   6.794 0.00173 **
    Residuals   98  5.744  0.0586                   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic$LR_amplit_erotic, Pupil_erotic$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic$LR_amplit_erotic and Pupil_erotic$Group2 

       Men    NC    
    NC 0.0015 -     
    OC 0.0300 0.2965

    P value adjustment method: holm 

``` r
  LR_latency_erotic <- aov(LR_latency_erotic ~ Group2, data = Pupil_erotic)
summary(LR_latency_erotic)
```

                Df  Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  152343   76172   2.632  0.077 .
    Residuals   98 2836072   28940                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic$LR_latency_erotic, Pupil_erotic$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic$LR_latency_erotic and Pupil_erotic$Group2 

       Men  NC  
    NC 0.28 -   
    OC 0.44 0.08

    P value adjustment method: holm 

``` r
  Slope_erotic <- aov(Slope_erotic ~ Group2, data = Pupil_erotic)
summary(Slope_erotic)
```

                Df    Sum Sq   Mean Sq F value Pr(>F)
    Group2       2 7.730e-08 3.867e-08    2.26   0.11
    Residuals   98 1.677e-06 1.711e-08               

``` r
pairwise.t.test(Pupil_erotic$Slope_erotic, Pupil_erotic$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic$Slope_erotic and Pupil_erotic$Group2 

       Men  NC  
    NC 0.11 -   
    OC 0.51 0.51

    P value adjustment method: holm 

``` r
  AUC_erotic <- aov(AUC_erotic ~ Group2, data = Pupil_erotic)
summary(AUC_erotic)
```

                Df   Sum Sq Mean Sq F value Pr(>F)
    Group2       2   393956  196978   0.827   0.44
    Residuals   98 23339974  238163               

``` r
pairwise.t.test(Pupil_erotic$AUC_erotic, Pupil_erotic$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic$AUC_erotic and Pupil_erotic$Group2 

       Men  NC  
    NC 0.80 -   
    OC 0.80 0.97

    P value adjustment method: holm 

``` r
  Mean_diameter_erotic <- aov(Mean_diameter_erotic ~ Group2, data = Pupil_erotic)
summary(Mean_diameter_erotic)
```

                Df Sum Sq Mean Sq F value Pr(>F)  
    Group2       2   0.38 0.18988   4.028 0.0208 *
    Residuals   98   4.62 0.04714                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic$Mean_diameter_erotic, Pupil_erotic$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic$Mean_diameter_erotic and Pupil_erotic$Group2 

       Men   NC   
    NC 0.017 -    
    OC 0.239 0.229

    P value adjustment method: holm 
