# Androgenicity
Zelionkaite

## Libraries

``` r
library("tidyverse")
```

    Warning: paketas 'tidyverse' buvo sukurtas pagal R versiją 4.4.1

    Warning: paketas 'lubridate' buvo sukurtas pagal R versiją 4.4.1

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library("rstatix")
```


    Pridedamas paketas: 'rstatix'

    Šis objektas yra užmaskuotas nuo 'package:stats':

        filter

``` r
library("emmeans")
library("dplyr")
```

``` r
load("Androgenicity_data.RData")
```

# Gaze parameters - RM ANOVA

## Neutral

### Dwell time

``` r
## Only anti-androgenic OC-users
Gaze_neutral_anti%>%
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
    7 OC     Background  39.3  5.44
    8 OC     Body        26.9  5.53
    9 OC     Face        33.9  6.90

``` r
Neut_RM_ANOVA_dwell_time <- anova_test(Gaze_neutral_anti, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.853 0.000485     *
    2 Group2:AOI2 0.853 0.000485     *

``` r
Neut_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.872 1.74, 169.14 6.44e-22         * 0.886 1.77, 171.97 3.05e-22
    2 Group2:AOI2 0.872 3.49, 169.14 3.20e-02         * 0.886 3.55, 171.97 3.20e-02
      p[HF]<.05
    1         *
    2         *

``` r
get_anova_table(Neut_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd      F        p p<.05       pes
    1      Group2 2.00  97.00 -0.001 1.00e+00       -2.64e-05
    2        AOI2 1.74 169.14 74.597 6.44e-22     *  4.35e-01
    3 Group2:AOI2 3.49 169.14  2.833 3.20e-02     *  5.50e-02

``` r
#posthoc
Neut_RM_ANOVA_dwell_time_interact <- Gaze_neutral_anti %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 9 × 10
      AOI2    term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
    * <fct>   <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
    1 Backgr… Grou… dwel… Men    NC       291     2.09  0.0371  0.111   ns          
    2 Backgr… Grou… dwel… Men    OC       291     0.599 0.550   0.550   ns          
    3 Backgr… Grou… dwel… NC     OC       291    -1.30  0.195   0.389   ns          
    4 Body    Grou… dwel… Men    NC       291     0.950 0.343   0.686   ns          
    5 Body    Grou… dwel… Men    OC       291    -0.360 0.719   0.719   ns          
    6 Body    Grou… dwel… NC     OC       291    -1.21  0.226   0.677   ns          
    7 Face    Grou… dwel… Men    NC       291    -3.04  0.00255 0.00765 **          
    8 Face    Grou… dwel… Men    OC       291    -0.239 0.811   0.811   ns          
    9 Face    Grou… dwel… NC     OC       291     2.51  0.0125  0.0249  *           

``` r
Neut_RM_ANOVA_dwell_time_zonos <- Gaze_neutral_anti %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 3 × 9
      term  .y.        group1  group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>      <chr>   <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  dwell_time Backgr… Body     297     14.6  1.03e-36 3.09e-36 ****        
    2 AOI2  dwell_time Backgr… Face     297      4.23 3.16e- 5 3.16e- 5 ****        
    3 AOI2  dwell_time Body    Face     297    -10.4  1.13e-21 2.26e-21 ****        

``` r
Neut_RM_ANOVA_dwell_time_grupe <- Gaze_neutral_anti %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       297 -9.60e-11  1.00     1 ns          
    2 Group2 dwell_time Men    OC       297  3.20e-10  1.00     1 ns          
    3 Group2 dwell_time NC     OC       297  4.03e-10  1.00     1 ns          

``` r
### Without mini pill users
Gaze_neutral_mini%>%
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
    7 OC     Background  38.9  5.08
    8 OC     Body        26.8  5.63
    9 OC     Face        34.2  6.92

``` r
Neut_RM_ANOVA_dwell_time <- anova_test(Gaze_neutral_mini, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.852 0.000299     *
    2 Group2:AOI2 0.852 0.000299     *

``` r
Neut_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.871 1.74, 177.63 2.44e-23         * 0.885 1.77, 180.45 1.15e-23
    2 Group2:AOI2 0.871 3.48, 177.63 3.60e-02         * 0.885 3.54, 180.45 3.50e-02
      p[HF]<.05
    1         *
    2         *

``` r
get_anova_table(Neut_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd      F        p p<.05       pes
    1      Group2 2.00 102.00 -0.004 1.00e+00       -8.15e-05
    2        AOI2 1.74 177.63 80.149 2.44e-23     *  4.40e-01
    3 Group2:AOI2 3.48 177.63  2.765 3.60e-02     *  5.10e-02

``` r
#posthoc
Neut_RM_ANOVA_dwell_time_interact <- Gaze_neutral_mini %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 9 × 10
      AOI2    term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
    * <fct>   <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
    1 Backgr… Grou… dwel… Men    NC       306     2.10  0.0363  0.109   ns          
    2 Backgr… Grou… dwel… Men    OC       306     0.863 0.389   0.507   ns          
    3 Backgr… Grou… dwel… NC     OC       306    -1.14  0.253   0.507   ns          
    4 Body    Grou… dwel… Men    NC       306     0.954 0.341   0.681   ns          
    5 Body    Grou… dwel… Men    OC       306    -0.350 0.727   0.727   ns          
    6 Body    Grou… dwel… NC     OC       306    -1.25  0.212   0.635   ns          
    7 Face    Grou… dwel… Men    NC       306    -3.06  0.00243 0.00728 **          
    8 Face    Grou… dwel… Men    OC       306    -0.513 0.608   0.608   ns          
    9 Face    Grou… dwel… NC     OC       306     2.40  0.0172  0.0344  *           

``` r
Neut_RM_ANOVA_dwell_time_zonos <- Gaze_neutral_mini %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 3 × 9
      term  .y.        group1  group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>      <chr>   <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  dwell_time Backgr… Body     312     14.9  2.57e-38 7.72e-38 ****        
    2 AOI2  dwell_time Backgr… Face     312      4.20 3.49e- 5 3.49e- 5 ****        
    3 AOI2  dwell_time Body    Face     312    -10.7  5.46e-23 1.09e-22 ****        

``` r
Neut_RM_ANOVA_dwell_time_grupe <- Gaze_neutral_mini %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       312 -9.66e-11  1.00     1 ns          
    2 Group2 dwell_time Men    OC       312  3.21e-10  1.00     1 ns          
    3 Group2 dwell_time NC     OC       312  4.09e-10  1.00     1 ns          

### Return probability

``` r
## Only anti-androgenic OC-users
Gaze_neutral_anti%>%
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
    7 OC     Background 0.375 0.0376
    8 OC     Body       0.346 0.0478
    9 OC     Face       0.280 0.0571

``` r
Neut_RM_ANOVA_grizimo_tik <- anova_test(Gaze_neutral_anti, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_grizimo_tik$`Mauchly's Test for Sphericity`
```

           Effect     W     p p<.05
    1        AOI2 0.884 0.003     *
    2 Group2:AOI2 0.884 0.003     *

``` r
Neut_RM_ANOVA_grizimo_tik$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.896 1.79, 173.83 2.34e-19         * 0.912 1.82, 176.89 1.19e-19
    2 Group2:AOI2 0.896 3.58, 173.83 6.50e-02           0.912 3.65, 176.89 6.40e-02
      p[HF]<.05
    1         *
    2          

``` r
get_anova_table(Neut_RM_ANOVA_grizimo_tik)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd      F        p p<.05   pes
    1      Group2 2.00  97.00  0.238 7.89e-01       0.005
    2        AOI2 1.79 173.83 61.100 2.34e-19     * 0.386
    3 Group2:AOI2 3.58 173.83  2.329 6.50e-02       0.046

``` r
#posthoc
Neut_RM_ANOVA_grizimo_tik_interact <- Gaze_neutral_anti %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_interact)
```

    # A tibble: 9 × 10
      AOI2     term  .y.   group1 group2    df statistic       p  p.adj p.adj.signif
    * <fct>    <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>       
    1 Backgro… Grou… retu… Men    NC       291     1.11  0.269   0.808  ns          
    2 Backgro… Grou… retu… Men    OC       291     0.328 0.743   1.00   ns          
    3 Backgro… Grou… retu… NC     OC       291    -0.675 0.500   1.00   ns          
    4 Body     Grou… retu… Men    NC       291     1.64  0.101   0.296  ns          
    5 Body     Grou… retu… Men    OC       291    -0.173 0.863   0.863  ns          
    6 Body     Grou… retu… NC     OC       291    -1.66  0.0986  0.296  ns          
    7 Face     Grou… retu… Men    NC       291    -2.75  0.00631 0.0189 *           
    8 Face     Grou… retu… Men    OC       291    -0.156 0.877   0.877  ns          
    9 Face     Grou… retu… NC     OC       291     2.33  0.0204  0.0407 *           

``` r
Neut_RM_ANOVA_grizimo_tik_zonos <- Gaze_neutral_anti %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_zonos)
```

    # A tibble: 3 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  return_prob Backg… Body     297      5.57 5.73e- 8 5.73e- 8 ****        
    2 AOI2  return_prob Backg… Face     297     13.4  3.15e-32 9.44e-32 ****        
    3 AOI2  return_prob Body   Face     297      7.80 1.07e-13 2.14e-13 ****        

``` r
Neut_RM_ANOVA_grizimo_tik_grupe <- Gaze_neutral_anti %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df     statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>         <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       297 0.00000000102  1.00     1 ns          
    2 Group2 return_prob Men    OC       297 0.00000000927  1.00     1 ns          
    3 Group2 return_prob NC     OC       297 0.00000000824  1.00     1 ns          

``` r
### Without mini pill users
Gaze_neutral_mini%>%
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
    7 OC     Background 0.376 0.0352
    8 OC     Body       0.345 0.0483
    9 OC     Face       0.279 0.0558

``` r
Neut_RM_ANOVA_grizimo_tik <- anova_test(Gaze_neutral_mini, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_grizimo_tik$`Mauchly's Test for Sphericity`
```

           Effect     W     p p<.05
    1        AOI2 0.873 0.001     *
    2 Group2:AOI2 0.873 0.001     *

``` r
Neut_RM_ANOVA_grizimo_tik$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.887 1.77, 180.94 1.08e-20         * 0.902  1.8, 183.91 5.43e-21
    2 Group2:AOI2 0.887 3.55, 180.94 5.70e-02           0.902 3.61, 183.91 5.60e-02
      p[HF]<.05
    1         *
    2          

``` r
get_anova_table(Neut_RM_ANOVA_grizimo_tik)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd      F        p p<.05   pes
    1      Group2 2.00 102.00  0.242 7.86e-01       0.005
    2        AOI2 1.77 180.94 66.653 1.08e-20     * 0.395
    3 Group2:AOI2 3.55 180.94  2.420 5.70e-02       0.045

``` r
#posthoc
Neut_RM_ANOVA_grizimo_tik_interact <- Gaze_neutral_mini %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_interact)
```

    # A tibble: 9 × 10
      AOI2     term  .y.   group1 group2    df statistic       p  p.adj p.adj.signif
    * <fct>    <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>       
    1 Backgro… Grou… retu… Men    NC       306     1.11  0.268   0.804  ns          
    2 Backgro… Grou… retu… Men    OC       306     0.242 0.809   0.833  ns          
    3 Backgro… Grou… retu… NC     OC       306    -0.814 0.417   0.833  ns          
    4 Body     Grou… retu… Men    NC       306     1.65  0.100   0.268  ns          
    5 Body     Grou… retu… Men    OC       306    -0.141 0.888   0.888  ns          
    6 Body     Grou… retu… NC     OC       306    -1.70  0.0895  0.268  ns          
    7 Face     Grou… retu… Men    NC       306    -2.76  0.00617 0.0185 *           
    8 Face     Grou… retu… Men    OC       306    -0.102 0.919   0.919  ns          
    9 Face     Grou… retu… NC     OC       306     2.52  0.0123  0.0247 *           

``` r
Neut_RM_ANOVA_grizimo_tik_zonos <- Gaze_neutral_mini %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_zonos)
```

    # A tibble: 3 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  return_prob Backg… Body     312      5.75 2.18e- 8 2.18e- 8 ****        
    2 AOI2  return_prob Backg… Face     312     13.9  1.76e-34 5.28e-34 ****        
    3 AOI2  return_prob Body   Face     312      8.14 9.24e-15 1.85e-14 ****        

``` r
Neut_RM_ANOVA_grizimo_tik_grupe <- Gaze_neutral_mini %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_grizimo_tik_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df     statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>         <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       312 0.00000000102  1.00     1 ns          
    2 Group2 return_prob Men    OC       312 0.00000000868  1.00     1 ns          
    3 Group2 return_prob NC     OC       312 0.00000000761  1.00     1 ns          

### Looking probability

``` r
## Only anti-androgenic OC-users
Gaze_neutral_anti%>%
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
    7 OC     Background  0.535 0.0808
    8 OC     Body        1.70  0.373 
    9 OC     Face        9.80  3.97  

``` r
Neut_RM_ANOVA_looking_prob <- anova_test(Gaze_neutral_anti, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.024 1.45e-78     *
    2 Group2:AOI2 0.024 1.45e-78     *

``` r
Neut_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect   GGe      DF[GG]    p[GG] p[GG]<.05   HFe      DF[HF]    p[HF]
    1        AOI2 0.506 1.01, 98.17 2.64e-39         * 0.506 1.01, 98.21 2.55e-39
    2 Group2:AOI2 0.506 2.02, 98.17 1.12e-01           0.506 2.02, 98.21 1.12e-01
      p[HF]<.05
    1         *
    2          

``` r
get_anova_table(Neut_RM_ANOVA_looking_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn   DFd       F        p p<.05   pes
    1      Group2 2.00 97.00   2.240 1.12e-01       0.044
    2        AOI2 1.01 98.17 467.314 2.64e-39     * 0.828
    3 Group2:AOI2 2.02 98.17   2.236 1.12e-01       0.044

``` r
#posthoc
Neut_RM_ANOVA_looking_prob_interact <- Gaze_neutral_anti %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 9 × 10
      AOI2    term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
    * <fct>   <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
    1 Backgr… Grou… look… Men    NC       291    0.0744 9.41e-1 1   e+0 ns          
    2 Backgr… Grou… look… Men    OC       291    0.0221 9.82e-1 1   e+0 ns          
    3 Backgr… Grou… look… NC     OC       291   -0.0453 9.64e-1 1   e+0 ns          
    4 Body    Grou… look… Men    NC       291    0.0191 9.85e-1 1   e+0 ns          
    5 Body    Grou… look… Men    OC       291   -0.0911 9.27e-1 1   e+0 ns          
    6 Body    Grou… look… NC     OC       291   -0.107  9.15e-1 1   e+0 ns          
    7 Face    Grou… look… Men    NC       291   -1.93   5.43e-2 1.09e-1 ns          
    8 Face    Grou… look… Men    OC       291    1.93   5.51e-2 1.09e-1 ns          
    9 Face    Grou… look… NC     OC       291    3.65   3.09e-4 9.28e-4 ***         

``` r
Neut_RM_ANOVA_looking_prob_zonos <- Gaze_neutral_anti %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  looking_pr… Backg… Body     297     -3.04 2.58e- 3 2.58e- 3 **          
    2 AOI2  looking_pr… Backg… Face     297    -28.8  5.34e-88 1.60e-87 ****        
    3 AOI2  looking_pr… Body   Face     297    -25.8  1.02e-77 2.04e-77 ****        

``` r
Neut_RM_ANOVA_looking_prob_grupe <- Gaze_neutral_anti %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       297    -0.502 0.616     1 ns          
    2 Group2 looking_prob Men    OC       297     0.507 0.612     1 ns          
    3 Group2 looking_prob NC     OC       297     0.955 0.340     1 ns          

``` r
### Without mini pill users
Gaze_neutral_mini%>%
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
    7 OC     Background  0.530 0.0751
    8 OC     Body        1.68  0.370 
    9 OC     Face       10.2   3.97  

``` r
Neut_RM_ANOVA_looking_prob <- anova_test(Gaze_neutral_mini, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neut_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect     W       p p<.05
    1        AOI2 0.023 3.9e-83     *
    2 Group2:AOI2 0.023 3.9e-83     *

``` r
Neut_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect   GGe      DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.506 1.01, 103.2 3.16e-42         * 0.506 1.01, 103.24 3.06e-42
    2 Group2:AOI2 0.506 2.02, 103.2 1.73e-01           0.506 2.02, 103.24 1.73e-01
      p[HF]<.05
    1         *
    2          

``` r
get_anova_table(Neut_RM_ANOVA_looking_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn   DFd       F        p p<.05   pes
    1      Group2 2.00 102.0   1.819 1.67e-01       0.034
    2        AOI2 1.01 103.2 518.043 3.16e-42     * 0.835
    3 Group2:AOI2 2.02 103.2   1.782 1.73e-01       0.034

``` r
#posthoc
Neut_RM_ANOVA_looking_prob_interact <- Gaze_neutral_mini %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 9 × 10
      AOI2    term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
    * <fct>   <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
    1 Backgr… Grou… look… Men    NC       306    0.0747 0.940   1       ns          
    2 Backgr… Grou… look… Men    OC       306    0.0318 0.975   1       ns          
    3 Backgr… Grou… look… NC     OC       306   -0.0395 0.969   1       ns          
    4 Body    Grou… look… Men    NC       306    0.0192 0.985   1       ns          
    5 Body    Grou… look… Men    OC       306   -0.0632 0.950   1       ns          
    6 Body    Grou… look… NC     OC       306   -0.0807 0.936   1       ns          
    7 Face    Grou… look… Men    NC       306   -1.94   0.0531  0.106   ns          
    8 Face    Grou… look… Men    OC       306    1.43   0.155   0.155   ns          
    9 Face    Grou… look… NC     OC       306    3.25   0.00127 0.00381 **          

``` r
Neut_RM_ANOVA_looking_prob_zonos <- Gaze_neutral_mini %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  looking_pr… Backg… Body     312     -3.14 1.87e- 3 1.87e- 3 **          
    2 AOI2  looking_pr… Backg… Face     312    -29.9  9.42e-94 2.83e-93 ****        
    3 AOI2  looking_pr… Body   Face     312    -26.8  6.09e-83 1.22e-82 ****        

``` r
Neut_RM_ANOVA_looking_prob_grupe <- Gaze_neutral_mini %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Neut_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       312    -0.501 0.616     1 ns          
    2 Group2 looking_prob Men    OC       312     0.379 0.705     1 ns          
    3 Group2 looking_prob NC     OC       312     0.850 0.396     1 ns          

## Negative

### Dwell time

``` r
## Only anti-androgenic OC-users

Gaze_negative_anti%>%
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
    7 OC     Background   11.4  2.99
    8 OC     Intact       11.0  3.38
    9 OC     Mutilations  77.6  5.20

``` r
Neig_RM_ANOVA_dwell_time <- anova_test(Gaze_negative_anti, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.365 1.58e-21     *
    2 Group2:AOI2 0.365 1.58e-21     *

``` r
Neig_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]     p[GG] p[GG]<.05   HFe       DF[HF]
    1        AOI2 0.612 1.22, 117.42 8.65e-109         * 0.615 1.23, 118.15
    2 Group2:AOI2 0.612 2.45, 117.42  5.50e-01           0.615 2.46, 118.15
          p[HF] p[HF]<.05
    1 1.91e-109         *
    2  5.51e-01          

``` r
Neig_RM_ANOVA_dwell_time$`emmeans`
```

    NULL

``` r
get_anova_table(Neig_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd           F         p p<.05      pes
    1      Group2 2.00  96.00    0.000426  1.00e+00       8.88e-06
    2        AOI2 1.22 117.42 6337.420000 8.65e-109     * 9.85e-01
    3 Group2:AOI2 2.45 117.42    0.657000  5.50e-01       1.40e-02

``` r
#posthoc
Neig_RM_ANOVA_dwell_time_interact <- Gaze_negative_anti %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 9 × 10
      AOI2        term  .y.   group1 group2    df statistic     p p.adj p.adj.signif
    * <fct>       <chr> <chr> <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Background  Grou… dwel… Men    NC       288     0.975 0.331 0.992 ns          
    2 Background  Grou… dwel… Men    OC       288     0.231 0.818 1     ns          
    3 Background  Grou… dwel… NC     OC       288    -0.643 0.521 1     ns          
    4 Intact      Grou… dwel… Men    NC       288     0.508 0.612 1     ns          
    5 Intact      Grou… dwel… Men    OC       288    -0.110 0.912 1     ns          
    6 Intact      Grou… dwel… NC     OC       288    -0.563 0.574 1     ns          
    7 Mutilations Grou… dwel… Men    NC       288    -1.48  0.139 0.418 ns          
    8 Mutilations Grou… dwel… Men    OC       288    -0.120 0.904 0.904 ns          
    9 Mutilations Grou… dwel… NC     OC       288     1.21  0.229 0.458 ns          

``` r
Neig_RM_ANOVA_dwell_time_zonos <- Gaze_negative_anti %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  dwell_ti… Backg… Intact   294     0.839 4.02e-  1 4.02e-  1 ns          
    2 AOI2  dwell_ti… Backg… Mutil…   294  -122.    2.07e-253 4.15e-253 ****        
    3 AOI2  dwell_ti… Intact Mutil…   294  -122.    2.85e-254 8.56e-254 ****        

``` r
Neig_RM_ANOVA_dwell_time_grupe <- Gaze_negative_anti %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       294  4.66e-11  1.00     1 ns          
    2 Group2 dwell_time Men    OC       294  6.48e-11  1.00     1 ns          
    3 Group2 dwell_time NC     OC       294  2.24e-11  1.00     1 ns          

``` r
### Without mini pill users

Gaze_negative_mini%>%
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
    7 OC     Background   11.7  3.07
    8 OC     Intact       11.8  4.46
    9 OC     Mutilations  76.5  6.24

``` r
Neig_RM_ANOVA_dwell_time <- anova_test(Gaze_negative_mini, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect    W       p p<.05
    1        AOI2 0.37 4.3e-22     *
    2 Group2:AOI2 0.37 4.3e-22     *

``` r
Neig_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]     p[GG] p[GG]<.05   HFe       DF[HF]
    1        AOI2 0.614 1.23, 122.71 1.83e-110         * 0.617 1.23, 123.45
    2 Group2:AOI2 0.614 2.45, 122.71  2.60e-01           0.617 2.47, 123.45
         p[HF] p[HF]<.05
    1 4.1e-111         *
    2  2.6e-01          

``` r
Neig_RM_ANOVA_dwell_time$`emmeans`
```

    NULL

``` r
get_anova_table(Neig_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd        F         p p<.05      pes
    1      Group2 2.00 100.00    0.001  9.99e-01       2.77e-05
    2        AOI2 1.23 122.71 5852.063 1.83e-110     * 9.83e-01
    3 Group2:AOI2 2.45 122.71    1.361  2.60e-01       2.70e-02

``` r
#posthoc
Neig_RM_ANOVA_dwell_time_interact <- Gaze_negative_mini %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 9 × 10
      AOI2      term  .y.   group1 group2    df statistic      p  p.adj p.adj.signif
    * <fct>     <chr> <chr> <chr>  <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>       
    1 Backgrou… Grou… dwel… Men    NC       300    0.916  0.360  1      ns          
    2 Backgrou… Grou… dwel… Men    OC       300   -0.0854 0.932  1      ns          
    3 Backgrou… Grou… dwel… NC     OC       300   -0.938  0.349  1      ns          
    4 Intact    Grou… dwel… Men    NC       300    0.478  0.633  0.753  ns          
    5 Intact    Grou… dwel… Men    OC       300   -0.886  0.377  0.753  ns          
    6 Intact    Grou… dwel… NC     OC       300   -1.32   0.188  0.563  ns          
    7 Mutilati… Grou… dwel… Men    NC       300   -1.39   0.164  0.329  ns          
    8 Mutilati… Grou… dwel… Men    OC       300    0.971  0.332  0.332  ns          
    9 Mutilati… Grou… dwel… NC     OC       300    2.26   0.0246 0.0738 ns          

``` r
Neig_RM_ANOVA_dwell_time_zonos <- Gaze_negative_mini %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  dwell_ti… Backg… Intact   306     0.561 5.75e-  1 5.75e-  1 ns          
    2 AOI2  dwell_ti… Backg… Mutil…   306  -115.    7.05e-254 1.41e-253 ****        
    3 AOI2  dwell_ti… Intact Mutil…   306  -116.    1.65e-254 4.94e-254 ****        

``` r
Neig_RM_ANOVA_dwell_time_grupe <- Gaze_negative_mini %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       306  4.69e-11  1.00     1 ns          
    2 Group2 dwell_time Men    OC       306  9.11e-11  1.00     1 ns          
    3 Group2 dwell_time NC     OC       306  4.62e-11  1.00     1 ns          

### Return probability

``` r
## Only anti-androgenic OC-users
Gaze_negative_anti%>%
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
    7 OC     Background  0.190 0.0415
    8 OC     Intact      0.154 0.0446
    9 OC     Mutilations 0.657 0.0615

``` r
Neig_RM_ANOVA_return_prob <- anova_test(Gaze_negative_anti, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_return_prob$`Mauchly's Test for Sphericity`
```

           Effect    W        p p<.05
    1        AOI2 0.77 3.96e-06     *
    2 Group2:AOI2 0.77 3.96e-06     *

``` r
Neig_RM_ANOVA_return_prob$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]     p[GG] p[GG]<.05   HFe       DF[HF]
    1        AOI2 0.813 1.63, 156.05 1.88e-103         * 0.825 1.65, 158.36
    2 Group2:AOI2 0.813 3.25, 156.05  6.90e-02           0.825  3.3, 158.36
          p[HF] p[HF]<.05
    1 6.14e-105         *
    2  6.80e-02          

``` r
Neig_RM_ANOVA_return_prob$`emmeans`
```

    NULL

``` r
get_anova_table(Neig_RM_ANOVA_return_prob)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd        F         p p<.05      pes
    1      Group2 2.00  96.00    0.027  9.73e-01       0.000569
    2        AOI2 1.63 156.05 1869.987 1.88e-103     * 0.951000
    3 Group2:AOI2 3.25 156.05    2.358  6.90e-02       0.047000

``` r
#posthoc
Neig_RM_ANOVA_return_prob_interact <- Gaze_negative_anti %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_interact)
```

    # A tibble: 9 × 10
      AOI2     term  .y.   group1 group2    df statistic       p  p.adj p.adj.signif
    * <fct>    <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>       
    1 Backgro… Grou… retu… Men    NC       288     2.21  0.0280  0.0839 ns          
    2 Backgro… Grou… retu… Men    OC       288     1.15  0.250   0.499  ns          
    3 Backgro… Grou… retu… NC     OC       288    -0.832 0.406   0.499  ns          
    4 Intact   Grou… retu… Men    NC       288     0.472 0.637   1      ns          
    5 Intact   Grou… retu… Men    OC       288     0.968 0.334   1      ns          
    6 Intact   Grou… retu… NC     OC       288     0.536 0.592   1      ns          
    7 Mutilat… Grou… retu… Men    NC       288    -2.68  0.00777 0.0233 *           
    8 Mutilat… Grou… retu… Men    OC       288    -2.12  0.0347  0.0694 ns          
    9 Mutilat… Grou… retu… NC     OC       288     0.296 0.767   0.767  ns          

``` r
Neig_RM_ANOVA_return_prob_zonos <- Gaze_negative_anti %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  return_p… Backg… Intact   294      4.18 3.85e-  5 3.85e-  5 ****        
    2 AOI2  return_p… Backg… Mutil…   294    -62.6  4.42e-172 8.84e-172 ****        
    3 AOI2  return_p… Intact Mutil…   294    -66.8  8.64e-180 2.59e-179 ****        

``` r
Neig_RM_ANOVA_return_prob_grupe <- Gaze_negative_anti %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       294 -3.32e-10  1.00     1 ns          
    2 Group2 return_prob Men    OC       294  1.23e-10  1.00     1 ns          
    3 Group2 return_prob NC     OC       294  4.19e-10  1.00     1 ns          

``` r
### Without mini pill users
Gaze_negative_mini%>%
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
    7 OC     Background  0.191 0.0400
    8 OC     Intact      0.159 0.0460
    9 OC     Mutilations 0.649 0.0641

``` r
Neig_RM_ANOVA_return_prob <- anova_test(Gaze_negative_mini, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_return_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.748 5.83e-07     *
    2 Group2:AOI2 0.748 5.83e-07     *

``` r
Neig_RM_ANOVA_return_prob$`Sphericity Corrections`
```

           Effect   GGe      DF[GG]     p[GG] p[GG]<.05  HFe       DF[HF]     p[HF]
    1        AOI2 0.799 1.6, 159.78 1.19e-105         * 0.81 1.62, 161.96 4.68e-107
    2 Group2:AOI2 0.799 3.2, 159.78  1.02e-01           0.81 3.24, 161.96  1.01e-01
      p[HF]<.05
    1         *
    2          

``` r
Neig_RM_ANOVA_return_prob$`emmeans`
```

    NULL

``` r
get_anova_table(Neig_RM_ANOVA_return_prob)
```

    ANOVA Table (type III tests)

           Effect DFn    DFd        F         p p<.05      pes
    1      Group2 2.0 100.00    0.006  9.94e-01       0.000126
    2        AOI2 1.6 159.78 1931.476 1.19e-105     * 0.951000
    3 Group2:AOI2 3.2 159.78    2.074  1.02e-01       0.040000

``` r
#posthoc
Neig_RM_ANOVA_return_prob_interact <- Gaze_negative_mini %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_interact)
```

    # A tibble: 9 × 10
      AOI2     term  .y.   group1 group2    df statistic       p  p.adj p.adj.signif
    * <fct>    <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>       
    1 Backgro… Grou… retu… Men    NC       300    2.20   0.0288  0.0865 ns          
    2 Backgro… Grou… retu… Men    OC       300    1.06   0.291   0.583  ns          
    3 Backgro… Grou… retu… NC     OC       300   -1.00   0.317   0.583  ns          
    4 Intact   Grou… retu… Men    NC       300    0.469  0.639   1      ns          
    5 Intact   Grou… retu… Men    OC       300    0.529  0.597   1      ns          
    6 Intact   Grou… retu… NC     OC       300    0.0853 0.932   1      ns          
    7 Mutilat… Grou… retu… Men    NC       300   -2.67   0.00811 0.0243 *           
    8 Mutilat… Grou… retu… Men    OC       300   -1.59   0.114   0.228  ns          
    9 Mutilat… Grou… retu… NC     OC       300    0.917  0.360   0.360  ns          

``` r
Neig_RM_ANOVA_return_prob_zonos <- Gaze_negative_mini %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  return_p… Backg… Intact   306      4.12 4.91e-  5 4.91e-  5 ****        
    2 AOI2  return_p… Backg… Mutil…   306    -63.4  4.32e-178 8.64e-178 ****        
    3 AOI2  return_p… Intact Mutil…   306    -67.5  6.76e-186 2.03e-185 ****        

``` r
Neig_RM_ANOVA_return_prob_grupe <- Gaze_negative_mini %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_return_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       306 -3.34e-10  1.00     1 ns          
    2 Group2 return_prob Men    OC       306 -1.68e-10  1.00     1 ns          
    3 Group2 return_prob NC     OC       306  1.46e-10  1.00     1 ns          

### Looking probability

``` r
## Only anti-androgenic OC-users
Gaze_negative_anti%>%
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
    7 OC     Background   0.217 0.0568
    8 OC     Intact       3.71  1.94  
    9 OC     Mutilations 15.5   4.22  

``` r
Neig_RM_ANOVA_looking_prob <- anova_test(Gaze_negative_anti, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.518 2.66e-14     *
    2 Group2:AOI2 0.518 2.66e-14     *

``` r
Neig_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.675 1.35, 129.54 1.10e-72         * 0.681 1.36, 130.73 2.52e-73
    2 Group2:AOI2 0.675  2.7, 129.54 3.48e-01           0.681 2.72, 130.73 3.49e-01
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
    1      Group2 2.00  96.00    1.504 2.27e-01       0.030
    2        AOI2 1.35 129.54 1112.847 1.10e-72     * 0.921
    3 Group2:AOI2 2.70 129.54    1.100 3.48e-01       0.022

``` r
#posthoc
Neig_RM_ANOVA_looking_prob_interact <- Gaze_negative_anti %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 9 × 10
      AOI2     term  .y.   group1 group2    df statistic       p  p.adj p.adj.signif
    * <fct>    <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>  <dbl> <chr>       
    1 Backgro… Grou… look… Men    NC       288    0.0378 0.970   1      ns          
    2 Backgro… Grou… look… Men    OC       288    0.0147 0.988   1      ns          
    3 Backgro… Grou… look… NC     OC       288   -0.0192 0.985   1      ns          
    4 Intact   Grou… look… Men    NC       288   -0.0856 0.932   1      ns          
    5 Intact   Grou… look… Men    OC       288   -0.360  0.719   1      ns          
    6 Intact   Grou… look… NC     OC       288   -0.279  0.780   1      ns          
    7 Mutilat… Grou… look… Men    NC       288   -1.77   0.0786  0.157  ns          
    8 Mutilat… Grou… look… Men    OC       288   -2.61   0.00962 0.0289 *           
    9 Mutilat… Grou… look… NC     OC       288   -1.00   0.317   0.317  ns          

``` r
Neig_RM_ANOVA_looking_prob_zonos <- Gaze_negative_anti %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  looking_… Backg… Intact   294     -10.5 4.39e- 22 4.39e- 22 ****        
    2 AOI2  looking_… Backg… Mutil…   294     -45.4 6.96e-135 2.09e-134 ****        
    3 AOI2  looking_… Intact Mutil…   294     -34.9 1.34e-106 2.67e-106 ****        

``` r
Neig_RM_ANOVA_looking_prob_grupe <- Gaze_negative_anti %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       294    -0.354 0.723     1 ns          
    2 Group2 looking_prob Men    OC       294    -0.577 0.564     1 ns          
    3 Group2 looking_prob NC     OC       294    -0.254 0.799     1 ns          

``` r
### Without mini pill users
Gaze_negative_mini%>%
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
    7 OC     Background   0.226 0.0643
    8 OC     Intact       3.95  2.90  
    9 OC     Mutilations 15.1   4.10  

``` r
Neig_RM_ANOVA_looking_prob <- anova_test(Gaze_negative_mini, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Neig_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.594 6.39e-12     *
    2 Group2:AOI2 0.594 6.39e-12     *

``` r
Neig_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.711 1.42, 142.26 1.34e-76         * 0.719 1.44, 143.72 2.32e-77
    2 Group2:AOI2 0.711 2.85, 142.26 5.87e-01           0.719 2.87, 143.72 5.89e-01
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
    1      Group2 2.00 100.00    1.228 2.97e-01       0.024
    2        AOI2 1.42 142.26 1042.498 1.34e-76     * 0.912
    3 Group2:AOI2 2.85 142.26    0.633 5.87e-01       0.012

``` r
#posthoc
Neig_RM_ANOVA_looking_prob_interact <- Gaze_negative_mini %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 9 × 10
      AOI2       term  .y.   group1 group2    df statistic      p p.adj p.adj.signif
    * <fct>      <chr> <chr> <chr>  <chr>  <dbl>     <dbl>  <dbl> <dbl> <chr>       
    1 Background Grou… look… Men    NC       300  0.0363   0.971  1     ns          
    2 Background Grou… look… Men    OC       300 -0.000959 0.999  1     ns          
    3 Background Grou… look… NC     OC       300 -0.0348   0.972  1     ns          
    4 Intact     Grou… look… Men    NC       300 -0.0822   0.935  1     ns          
    5 Intact     Grou… look… Men    OC       300 -0.772    0.441  1     ns          
    6 Intact     Grou… look… NC     OC       300 -0.686    0.493  1     ns          
    7 Mutilatio… Grou… look… Men    NC       300 -1.69     0.0912 0.208 ns          
    8 Mutilatio… Grou… look… Men    OC       300 -1.82     0.0694 0.208 ns          
    9 Mutilatio… Grou… look… NC     OC       300 -0.222    0.825  0.825 ns          

``` r
Neig_RM_ANOVA_looking_prob_zonos <- Gaze_negative_mini %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 3 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  looking_… Backg… Intact   306     -10.5 2.49e- 22 2.49e- 22 ****        
    2 AOI2  looking_… Backg… Mutil…   306     -44.3 3.05e-135 9.15e-135 ****        
    3 AOI2  looking_… Intact Mutil…   306     -33.8 2.44e-105 4.89e-105 ****        

``` r
Neig_RM_ANOVA_looking_prob_grupe <- Gaze_negative_mini %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Neig_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       306    -0.356 0.722     1 ns          
    2 Group2 looking_prob Men    OC       306    -0.531 0.596     1 ns          
    3 Group2 looking_prob NC     OC       306    -0.193 0.847     1 ns          

## Erotic

### Dwell time

``` r
## Only anti-androgenic OC-users
Gaze_erotic_anti%>%
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
     9 OC     Background  4.74  3.73
    10 OC     Body       35.8   5.74
    11 OC     Erogenous  10.4   3.63
    12 OC     Face       49.0   5.13

``` r
Teig_RM_ANOVA_dwell_time <- anova_test(Gaze_erotic_anti, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.378 1.49e-17     *
    2 Group2:AOI2 0.378 1.49e-17     *

``` r
Teig_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect  GGe       DF[GG]     p[GG] p[GG]<.05   HFe      DF[HF]     p[HF]
    1        AOI2 0.61 1.83, 168.46 1.39e-107         * 0.622 1.87, 171.7 1.33e-109
    2 Group2:AOI2 0.61 3.66, 168.46  8.99e-04         * 0.622 3.73, 171.7  8.22e-04
      p[HF]<.05
    1         *
    2         *

``` r
Teig_RM_ANOVA_dwell_time$`emmeans`
```

    NULL

``` r
get_anova_table(Teig_RM_ANOVA_dwell_time)
```

    ANOVA Table (type III tests)

           Effect  DFn    DFd            F         p p<.05       pes
    1      Group2 2.00  92.00   -0.0000516  1.00e+00       -1.12e-06
    2        AOI2 1.83 168.46 1607.3510000 1.39e-107     *  9.46e-01
    3 Group2:AOI2 3.66 168.46    5.1540000  8.99e-04     *  1.01e-01

``` r
#posthoc
Teig_RM_ANOVA_dwell_time_interact <- Gaze_erotic_anti %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… dwel… Men    NC       368   -0.0576 9.54e-1 9.54e-1 ns          
     2 Backg… Grou… dwel… Men    OC       368   -1.17   2.42e-1 7.27e-1 ns          
     3 Backg… Grou… dwel… NC     OC       368   -1.12   2.61e-1 7.27e-1 ns          
     4 Body   Grou… dwel… Men    NC       368    2.14   3.34e-2 7.36e-2 ns          
     5 Body   Grou… dwel… Men    OC       368   -0.270  7.87e-1 7.87e-1 ns          
     6 Body   Grou… dwel… NC     OC       368   -2.26   2.45e-2 7.36e-2 ns          
     7 Eroge… Grou… dwel… Men    NC       368    2.56   1.09e-2 3.27e-2 *           
     8 Eroge… Grou… dwel… Men    OC       368    1.74   8.26e-2 1.65e-1 ns          
     9 Eroge… Grou… dwel… NC     OC       368   -0.628  5.30e-1 5.30e-1 ns          
    10 Face   Grou… dwel… Men    NC       368   -4.64   4.93e-6 1.48e-5 ****        
    11 Face   Grou… dwel… Men    OC       368   -0.300  7.65e-1 7.65e-1 ns          
    12 Face   Grou… dwel… NC     OC       368    4.01   7.33e-5 1.47e-4 ***         

``` r
Teig_RM_ANOVA_dwell_time_zonos <- Gaze_erotic_anti %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 6 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  dwell_ti… Backg… Body     376     -45.6 3.47e-155 1.39e-154 ****        
    2 AOI2  dwell_ti… Backg… Eroge…   376     -10.5 1.13e- 22 1.13e- 22 ****        
    3 AOI2  dwell_ti… Backg… Face     376     -68.8 3.39e-215 2.04e-214 ****        
    4 AOI2  dwell_ti… Body   Eroge…   376      35.1 1.05e-120 3.16e-120 ****        
    5 AOI2  dwell_ti… Body   Face     376     -23.3 8.64e- 75 1.73e- 74 ****        
    6 AOI2  dwell_ti… Eroge… Face     376     -58.4 1.45e-190 7.26e-190 ****        

``` r
Teig_RM_ANOVA_dwell_time_grupe <- Gaze_erotic_anti %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       377 -9.92e-12  1.00     1 ns          
    2 Group2 dwell_time Men    OC       377 -3.86e-11  1.00     1 ns          
    3 Group2 dwell_time NC     OC       377 -2.96e-11  1.00     1 ns          

``` r
### Without mini pill users
Gaze_erotic_mini%>%
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
     9 OC     Background  4.44  3.53
    10 OC     Body       35.4   5.49
    11 OC     Erogenous  10.7   3.50
    12 OC     Face       49.5   5.14

``` r
Teig_RM_ANOVA_dwell_time <- anova_test(Gaze_erotic_mini, dv = dwell_time, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_dwell_time$`Mauchly's Test for Sphericity`
```

           Effect     W       p p<.05
    1        AOI2 0.373 8.5e-19     *
    2 Group2:AOI2 0.373 8.5e-19     *

``` r
Teig_RM_ANOVA_dwell_time$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]     p[GG] p[GG]<.05  HFe       DF[HF]
    1        AOI2 0.609 1.83, 177.13 2.45e-114         * 0.62 1.86, 180.35
    2 Group2:AOI2 0.609 3.65, 177.13  1.00e-03         * 0.62 3.72, 180.35
          p[HF] p[HF]<.05
    1 2.31e-116         *
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
    1      Group2 2.00  97.00   -0.0000637  1.00e+00       -1.31e-06
    2        AOI2 1.83 177.13 1754.0050000 2.45e-114     *  9.48e-01
    3 Group2:AOI2 3.65 177.13    4.8540000  1.00e-03     *  9.10e-02

``` r
#posthoc
Teig_RM_ANOVA_dwell_time_interact <- Gaze_erotic_mini %>%
  group_by(AOI2) %>%
  emmeans_test(dwell_time ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… dwel… Men    NC       388   -0.0580 9.54e-1 1   e+0 ns          
     2 Backg… Grou… dwel… Men    OC       388   -0.968  3.34e-1 1   e+0 ns          
     3 Backg… Grou… dwel… NC     OC       388   -0.918  3.59e-1 1   e+0 ns          
     4 Body   Grou… dwel… Men    NC       388    2.15   3.20e-2 9.59e-2 ns          
     5 Body   Grou… dwel… Men    OC       388    0.106  9.16e-1 9.16e-1 ns          
     6 Body   Grou… dwel… NC     OC       388   -2.00   4.67e-2 9.59e-2 ns          
     7 Eroge… Grou… dwel… Men    NC       388    2.58   1.03e-2 3.08e-2 *           
     8 Eroge… Grou… dwel… Men    OC       388    1.61   1.09e-1 2.19e-1 ns          
     9 Eroge… Grou… dwel… NC     OC       388   -0.902  3.68e-1 3.68e-1 ns          
    10 Face   Grou… dwel… Men    NC       388   -4.67   4.10e-6 1.23e-5 ****        
    11 Face   Grou… dwel… Men    OC       388   -0.742  4.58e-1 4.58e-1 ns          
    12 Face   Grou… dwel… NC     OC       388    3.82   1.58e-4 3.17e-4 ***         

``` r
Teig_RM_ANOVA_dwell_time_zonos <- Gaze_erotic_mini %>%
  emmeans_test(dwell_time ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_zonos)
```

    # A tibble: 6 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  dwell_ti… Backg… Body     396     -47.3 5.37e-165 2.15e-164 ****        
    2 AOI2  dwell_ti… Backg… Eroge…   396     -11.0 7.33e- 25 7.33e- 25 ****        
    3 AOI2  dwell_ti… Backg… Face     396     -71.7 6.50e-229 3.90e-228 ****        
    4 AOI2  dwell_ti… Body   Eroge…   396      36.3 6.51e-128 1.95e-127 ****        
    5 AOI2  dwell_ti… Body   Face     396     -24.4 6.68e- 81 1.34e- 80 ****        
    6 AOI2  dwell_ti… Eroge… Face     396     -60.7 1.42e-202 7.09e-202 ****        

``` r
Teig_RM_ANOVA_dwell_time_grupe <- Gaze_erotic_mini %>%
  emmeans_test(dwell_time ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_dwell_time_grupe)
```

    # A tibble: 3 × 9
      term   .y.        group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>      <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 dwell_time Men    NC       397 -9.92e-12  1.00     1 ns          
    2 Group2 dwell_time Men    OC       397  2.59e-11  1.00     1 ns          
    3 Group2 dwell_time NC     OC       397  3.57e-11  1.00     1 ns          

### Return probability

``` r
## Only anti-androgenic OC-users
Gaze_erotic_anti%>%
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
     9 OC     Background 0.0331 0.0251
    10 OC     Body       0.410  0.0678
    11 OC     Erogenous  0.111  0.0451
    12 OC     Face       0.446  0.0642

``` r
Teig_RM_ANOVA_return_prob <- anova_test(Gaze_erotic_anti, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_return_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.226 2.44e-27     *
    2 Group2:AOI2 0.226 2.44e-27     *

``` r
Teig_RM_ANOVA_return_prob$`Sphericity Corrections`
```

           Effect   GGe      DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.556 1.67, 153.4 2.89e-87         * 0.565 1.69, 155.89 1.23e-88
    2 Group2:AOI2 0.556 3.33, 153.4 1.20e-02         * 0.565 3.39, 155.89 1.10e-02
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
    1      Group2 2.00  92.0    0.898 4.11e-01       0.019
    2        AOI2 1.67 153.4 1130.813 2.89e-87     * 0.925
    3 Group2:AOI2 3.33 153.4    3.614 1.20e-02     * 0.073

``` r
#posthoc
Teig_RM_ANOVA_return_prob_interact <- Gaze_erotic_anti %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… retu… Men    NC       368    -0.211 8.33e-1 1   e+0 ns          
     2 Backg… Grou… retu… Men    OC       368    -0.632 5.28e-1 1   e+0 ns          
     3 Backg… Grou… retu… NC     OC       368    -0.439 6.61e-1 1   e+0 ns          
     4 Body   Grou… retu… Men    NC       368     2.29  2.23e-2 6.70e-2 ns          
     5 Body   Grou… retu… Men    OC       368     0.749 4.54e-1 4.54e-1 ns          
     6 Body   Grou… retu… NC     OC       368    -1.38  1.68e-1 3.37e-1 ns          
     7 Eroge… Grou… retu… Men    NC       368     2.16  3.16e-2 9.49e-2 ns          
     8 Eroge… Grou… retu… Men    OC       368     1.42  1.56e-1 3.12e-1 ns          
     9 Eroge… Grou… retu… NC     OC       368    -0.576 5.65e-1 5.65e-1 ns          
    10 Face   Grou… retu… Men    NC       368    -4.24  2.83e-5 8.49e-5 ****        
    11 Face   Grou… retu… Men    OC       368    -1.54  1.25e-1 1.25e-1 ns          
    12 Face   Grou… retu… NC     OC       368     2.39  1.71e-2 3.42e-2 *           

``` r
Teig_RM_ANOVA_return_prob_zonos <- Gaze_erotic_anti %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_zonos)
```

    # A tibble: 6 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  return_p… Backg… Body     376    -48.7  1.69e-164 8.46e-164 ****        
    2 AOI2  return_p… Backg… Eroge…   376    -11.3  1.28e- 25 2.56e- 25 ****        
    3 AOI2  return_p… Backg… Face     376    -54.3  3.65e-180 2.19e-179 ****        
    4 AOI2  return_p… Body   Eroge…   376     37.4  7.33e-129 2.20e-128 ****        
    5 AOI2  return_p… Body   Face     376     -5.64 3.36e-  8 3.36e-  8 ****        
    6 AOI2  return_p… Eroge… Face     376    -43.1  1.90e-147 7.59e-147 ****        

``` r
Teig_RM_ANOVA_return_prob_grupe <- Gaze_erotic_anti %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       377  -2.23e-9  1.00     1 ns          
    2 Group2 return_prob Men    OC       377  -4.59e-9  1.00     1 ns          
    3 Group2 return_prob NC     OC       377  -2.55e-9  1.00     1 ns          

``` r
### Without mini pill users
Gaze_erotic_mini%>%
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
     9 OC     Background 0.0301 0.0243
    10 OC     Body       0.408  0.0655
    11 OC     Erogenous  0.115  0.0440
    12 OC     Face       0.448  0.0639

``` r
Teig_RM_ANOVA_return_prob <- anova_test(Gaze_erotic_mini, dv = return_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_return_prob$`Mauchly's Test for Sphericity`
```

           Effect     W        p p<.05
    1        AOI2 0.226 5.64e-29     *
    2 Group2:AOI2 0.226 5.64e-29     *

``` r
Teig_RM_ANOVA_return_prob$`Sphericity Corrections`
```

           Effect   GGe       DF[GG]    p[GG] p[GG]<.05   HFe       DF[HF]    p[HF]
    1        AOI2 0.556 1.67, 161.91 6.39e-93         * 0.565 1.69, 164.41 2.61e-94
    2 Group2:AOI2 0.556 3.34, 161.91 1.20e-02         * 0.565 3.39, 164.41 1.20e-02
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

           Effect  DFn    DFd        F        p p<.05    pes
    1      Group2 2.00  97.00   -1.490 1.00e+00       -0.032
    2        AOI2 1.67 161.91 1224.583 6.39e-93     *  0.927
    3 Group2:AOI2 3.34 161.91    3.582 1.20e-02     *  0.069

``` r
#posthoc
Teig_RM_ANOVA_return_prob_interact <- Gaze_erotic_mini %>%
  group_by(AOI2) %>%
  emmeans_test(return_prob ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… retu… Men    NC       388    -0.213 8.32e-1 1   e+0 ns          
     2 Backg… Grou… retu… Men    OC       388    -0.434 6.65e-1 1   e+0 ns          
     3 Backg… Grou… retu… NC     OC       388    -0.229 8.19e-1 1   e+0 ns          
     4 Body   Grou… retu… Men    NC       388     2.31  2.15e-2 6.46e-2 ns          
     5 Body   Grou… retu… Men    OC       388     0.938 3.49e-1 3.83e-1 ns          
     6 Body   Grou… retu… NC     OC       388    -1.31  1.92e-1 3.83e-1 ns          
     7 Eroge… Grou… retu… Men    NC       388     2.17  3.06e-2 9.19e-2 ns          
     8 Eroge… Grou… retu… Men    OC       388     1.26  2.10e-1 4.20e-1 ns          
     9 Eroge… Grou… retu… NC     OC       388    -0.854 3.94e-1 4.20e-1 ns          
    10 Face   Grou… retu… Men    NC       388    -4.26  2.52e-5 7.55e-5 ****        
    11 Face   Grou… retu… Men    OC       388    -1.76  7.92e-2 7.92e-2 ns          
    12 Face   Grou… retu… NC     OC       388     2.39  1.72e-2 3.45e-2 *           

``` r
Teig_RM_ANOVA_return_prob_zonos <- Gaze_erotic_mini %>%
  emmeans_test(return_prob ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_zonos)
```

    # A tibble: 6 × 9
      term  .y.       group1 group2    df statistic         p     p.adj p.adj.signif
    * <chr> <chr>     <chr>  <chr>  <dbl>     <dbl>     <dbl>     <dbl> <chr>       
    1 AOI2  return_p… Backg… Body     396    -50.4  2.58e-174 1.29e-173 ****        
    2 AOI2  return_p… Backg… Eroge…   396    -11.9  6.06e- 28 1.21e- 27 ****        
    3 AOI2  return_p… Backg… Face     396    -56.3  4.16e-191 2.50e-190 ****        
    4 AOI2  return_p… Body   Eroge…   396     38.5  4.92e-136 1.48e-135 ****        
    5 AOI2  return_p… Body   Face     396     -5.93 6.65e-  9 6.65e-  9 ****        
    6 AOI2  return_p… Eroge… Face     396    -44.5  4.96e-156 1.98e-155 ****        

``` r
Teig_RM_ANOVA_return_prob_grupe <- Gaze_erotic_mini %>%
  emmeans_test(return_prob ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_return_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.         group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>       <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 return_prob Men    NC       397  -2.23e-9  1.00     1 ns          
    2 Group2 return_prob Men    OC       397  -4.29e-9  1.00     1 ns          
    3 Group2 return_prob NC     OC       397  -2.14e-9  1.00     1 ns          

### Looking probability

``` r
## Only anti-androgenic OC-users
Gaze_erotic_anti%>%
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
     9 OC     Background  0.119   0.104 
    10 OC     Body        3.13    0.746 
    11 OC     Erogenous  38.5    21.6   
    12 OC     Face        8.61    1.69  

``` r
Teig_RM_ANOVA_looking_prob <- anova_test(Gaze_erotic_anti, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect        W         p p<.05
    1        AOI2 0.000114 1.89e-175     *
    2 Group2:AOI2 0.000114 1.89e-175     *

``` r
Teig_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect  GGe      DF[GG]    p[GG] p[GG]<.05  HFe      DF[HF]    p[HF]
    1        AOI2 0.34 1.02, 93.76 3.50e-32         * 0.34 1.02, 93.82 3.35e-32
    2 Group2:AOI2 0.34 2.04, 93.76 1.25e-01           0.34 2.04, 93.82 1.25e-01
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
    1      Group2 2.00 92.00   1.463 2.37e-01       0.031
    2        AOI2 1.02 93.76 318.456 3.50e-32     * 0.776
    3 Group2:AOI2 2.04 93.76   2.116 1.25e-01       0.044

``` r
#posthoc
Teig_RM_ANOVA_looking_prob_interact <- Gaze_erotic_anti %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… look… Men    NC       368  0.000490 1.00e+0 1   e+0 ns          
     2 Backg… Grou… look… Men    OC       368 -0.0129   9.90e-1 1   e+0 ns          
     3 Backg… Grou… look… NC     OC       368 -0.0134   9.89e-1 1   e+0 ns          
     4 Body   Grou… look… Men    NC       368  0.0806   9.36e-1 1   e+0 ns          
     5 Body   Grou… look… Men    OC       368  0.0182   9.85e-1 1   e+0 ns          
     6 Body   Grou… look… NC     OC       368 -0.0567   9.55e-1 1   e+0 ns          
     7 Eroge… Grou… look… Men    NC       368  3.88     1.23e-4 3.68e-4 ***         
     8 Eroge… Grou… look… Men    OC       368  2.04     4.25e-2 8.50e-2 ns          
     9 Eroge… Grou… look… NC     OC       368 -1.56     1.19e-1 1.19e-1 ns          
    10 Face   Grou… look… Men    NC       368 -0.608    5.44e-1 1   e+0 ns          
    11 Face   Grou… look… Men    OC       368  0.0293   9.77e-1 1   e+0 ns          
    12 Face   Grou… look… NC     OC       368  0.595    5.52e-1 1   e+0 ns          

``` r
Teig_RM_ANOVA_looking_prob_zonos <- Gaze_erotic_anti %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 6 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  looking_pr… Backg… Body     376     -2.13 3.39e- 2 3.39e- 2 *           
    2 AOI2  looking_pr… Backg… Eroge…   376    -27.5  3.94e-92 2.36e-91 ****        
    3 AOI2  looking_pr… Backg… Face     376     -6.44 3.62e-10 1.09e- 9 ****        
    4 AOI2  looking_pr… Body   Eroge…   376    -25.4  1.43e-83 7.15e-83 ****        
    5 AOI2  looking_pr… Body   Face     376     -4.31 2.07e- 5 4.14e- 5 ****        
    6 AOI2  looking_pr… Eroge… Face     376     21.1  1.08e-65 4.31e-65 ****        

``` r
Teig_RM_ANOVA_looking_prob_grupe <- Gaze_erotic_anti %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       377     0.885 0.377     1 ns          
    2 Group2 looking_prob Men    OC       377     0.546 0.586     1 ns          
    3 Group2 looking_prob NC     OC       377    -0.274 0.785     1 ns          

``` r
### Without mini pill users
Gaze_erotic_mini%>%
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
     9 OC     Background  0.111   0.101 
    10 OC     Body        3.10    0.696 
    11 OC     Erogenous  37.5    20.8   
    12 OC     Face        8.70    1.74  

``` r
Teig_RM_ANOVA_looking_prob <- anova_test(Gaze_erotic_mini, dv = looking_prob, wid = id, within = AOI2, between = Group2, effect.size = "pes")

Teig_RM_ANOVA_looking_prob$`Mauchly's Test for Sphericity`
```

           Effect        W         p p<.05
    1        AOI2 0.000115 3.71e-185     *
    2 Group2:AOI2 0.000115 3.71e-185     *

``` r
Teig_RM_ANOVA_looking_prob$`Sphericity Corrections`
```

           Effect  GGe      DF[GG]    p[GG] p[GG]<.05  HFe      DF[HF]    p[HF]
    1        AOI2 0.34 1.02, 98.89 4.96e-34         * 0.34 1.02, 98.94 4.75e-34
    2 Group2:AOI2 0.34 2.04, 98.89 1.16e-01           0.34 2.04, 98.94 1.16e-01
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
    1      Group2 2.00 97.00   1.588 2.10e-01       0.032
    2        AOI2 1.02 98.89 339.260 4.96e-34     * 0.778
    3 Group2:AOI2 2.04 98.89   2.195 1.16e-01       0.043

``` r
#posthoc
Teig_RM_ANOVA_looking_prob_interact <- Gaze_erotic_mini %>%
  group_by(AOI2) %>%
  emmeans_test(looking_prob ~ Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_interact)
```

    # A tibble: 12 × 10
       AOI2   term  .y.   group1 group2    df statistic       p   p.adj p.adj.signif
     * <fct>  <chr> <chr> <chr>  <chr>  <dbl>     <dbl>   <dbl>   <dbl> <chr>       
     1 Backg… Grou… look… Men    NC       388  0.000494 1.00e+0 1   e+0 ns          
     2 Backg… Grou… look… Men    OC       388 -0.0106   9.92e-1 1   e+0 ns          
     3 Backg… Grou… look… NC     OC       388 -0.0111   9.91e-1 1   e+0 ns          
     4 Body   Grou… look… Men    NC       388  0.0812   9.35e-1 1   e+0 ns          
     5 Body   Grou… look… Men    OC       388  0.0324   9.74e-1 1   e+0 ns          
     6 Body   Grou… look… NC     OC       388 -0.0467   9.63e-1 1   e+0 ns          
     7 Eroge… Grou… look… Men    NC       388  3.91     1.08e-4 3.24e-4 ***         
     8 Eroge… Grou… look… Men    OC       388  2.58     1.04e-2 2.08e-2 *           
     9 Eroge… Grou… look… NC     OC       388 -1.23     2.21e-1 2.21e-1 ns          
    10 Face   Grou… look… Men    NC       388 -0.612    5.41e-1 1   e+0 ns          
    11 Face   Grou… look… Men    OC       388 -0.00873  9.93e-1 1   e+0 ns          
    12 Face   Grou… look… NC     OC       388  0.589    5.56e-1 1   e+0 ns          

``` r
Teig_RM_ANOVA_looking_prob_zonos <- Gaze_erotic_mini %>%
  emmeans_test(looking_prob ~ AOI2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_zonos)
```

    # A tibble: 6 × 9
      term  .y.         group1 group2    df statistic        p    p.adj p.adj.signif
    * <chr> <chr>       <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
    1 AOI2  looking_pr… Backg… Body     396     -2.20 2.87e- 2 2.87e- 2 *           
    2 AOI2  looking_pr… Backg… Eroge…   396    -28.2  9.33e-97 5.60e-96 ****        
    3 AOI2  looking_pr… Backg… Face     396     -6.66 9.28e-11 2.79e-10 ****        
    4 AOI2  looking_pr… Body   Eroge…   396    -26.0  1.08e-87 5.38e-87 ****        
    5 AOI2  looking_pr… Body   Face     396     -4.46 1.06e- 5 2.11e- 5 ****        
    6 AOI2  looking_pr… Eroge… Face     396     21.5  1.06e-68 4.25e-68 ****        

``` r
Teig_RM_ANOVA_looking_prob_grupe <- Gaze_erotic_mini %>%
  emmeans_test(looking_prob ~Group2, p.adjust.method = "holm")
print(Teig_RM_ANOVA_looking_prob_grupe)
```

    # A tibble: 3 × 9
      term   .y.          group1 group2    df statistic     p p.adj p.adj.signif
    * <chr>  <chr>        <chr>  <chr>  <dbl>     <dbl> <dbl> <dbl> <chr>       
    1 Group2 looking_prob Men    NC       397     0.892 0.373     1 ns          
    2 Group2 looking_prob Men    OC       397     0.683 0.495     1 ns          
    3 Group2 looking_prob NC     OC       397    -0.184 0.854     1 ns          

# Pupillary parameters

## Neutral

``` r
## Only anti-androgenic OC-users
LT_amplit_neurtral <- aov(LR_amplit_neutral ~ Group2, data = Pupil_neutral_anti)
summary(LT_amplit_neurtral)
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    Group2       2  0.688  0.3439   8.408 0.000429 ***
    Residuals   97  3.967  0.0409                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_anti$LR_amplit_neutral, Pupil_neutral_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_anti$LR_amplit_neutral and Pupil_neutral_anti$Group2 

       Men     NC     
    NC 0.00065 -      
    OC 0.00708 0.60667

    P value adjustment method: holm 

``` r
  LT_latency_neurtral <- aov(LR_latency_neutral ~ Group2, data = Pupil_neutral_anti)
summary(LT_latency_neurtral)
```

                Df  Sum Sq Mean Sq F value Pr(>F)
    Group2       2   61445   30722    0.42  0.658
    Residuals   97 7096382   73159               

``` r
pairwise.t.test(Pupil_neutral_anti$LR_latency_neutral, Pupil_neutral_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_anti$LR_latency_neutral and Pupil_neutral_anti$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  Slope_neutral <- aov(Slope_neutral ~ Group2, data = Pupil_neutral_anti)
summary(Slope_neutral)
```

                Df   Sum Sq   Mean Sq F value Pr(>F)  
    Group2       2 4.28e-08 2.142e-08   3.576 0.0317 *
    Residuals   97 5.81e-07 5.989e-09                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_anti$Slope_neutral, Pupil_neutral_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_anti$Slope_neutral and Pupil_neutral_anti$Group2 

       Men  NC  
    NC 0.03 -   
    OC 0.21 0.45

    P value adjustment method: holm 

``` r
  AUC_neutral <- aov(AUC_neutral ~ Group2, data = Pupil_neutral_anti)
summary(AUC_neutral)
```

                Df   Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  1095422  547711   4.645 0.0118 *
    Residuals   97 11438361  117921                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_anti$AUC_neutral, Pupil_neutral_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_anti$AUC_neutral and Pupil_neutral_anti$Group2 

       Men   NC   
    NC 0.018 -    
    OC 0.048 0.782

    P value adjustment method: holm 

``` r
  Mean_diameter_neutral <- aov(Mean_diameter_neutral ~ Group2, data = Pupil_neutral_anti)
summary(Mean_diameter_neutral)
```

                Df Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  0.360 0.17979   3.673  0.029 *
    Residuals   97  4.747 0.04894                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_anti$Mean_diameter_neutral, Pupil_neutral_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_anti$Mean_diameter_neutral and Pupil_neutral_anti$Group2 

       Men   NC   
    NC 0.025 -    
    OC 0.370 0.253

    P value adjustment method: holm 

``` r
### Without mini pill users
LT_amplit_neurtral <- aov(LR_amplit_neutral ~ Group2, data = Pupil_neutral_mini)
summary(LT_amplit_neurtral)
```

                 Df Sum Sq Mean Sq F value   Pr(>F)    
    Group2        2  0.746  0.3729   9.489 0.000166 ***
    Residuals   102  4.008  0.0393                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_mini$LR_amplit_neutral, Pupil_neutral_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_mini$LR_amplit_neutral and Pupil_neutral_mini$Group2 

       Men     NC     
    NC 0.00048 -      
    OC 0.00133 0.79998

    P value adjustment method: holm 

``` r
  LT_latency_neurtral <- aov(LR_latency_neutral ~ Group2, data = Pupil_neutral_mini)
summary(LT_latency_neurtral)
```

                 Df  Sum Sq Mean Sq F value Pr(>F)
    Group2        2   59900   29950   0.417   0.66
    Residuals   102 7322747   71792               

``` r
pairwise.t.test(Pupil_neutral_mini$LR_latency_neutral, Pupil_neutral_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_mini$LR_latency_neutral and Pupil_neutral_mini$Group2 

       Men NC
    NC 1   - 
    OC 1   1 

    P value adjustment method: holm 

``` r
  Slope_neutral <- aov(Slope_neutral ~ Group2, data = Pupil_neutral_mini)
summary(Slope_neutral)
```

                 Df    Sum Sq   Mean Sq F value Pr(>F)  
    Group2        2 4.530e-08 2.263e-08   3.785  0.026 *
    Residuals   102 6.098e-07 5.978e-09                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_mini$Slope_neutral, Pupil_neutral_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_mini$Slope_neutral and Pupil_neutral_mini$Group2 

       Men  NC  
    NC 0.03 -   
    OC 0.11 0.57

    P value adjustment method: holm 

``` r
  AUC_neutral <- aov(AUC_neutral ~ Group2, data = Pupil_neutral_mini)
summary(AUC_neutral)
```

                 Df   Sum Sq Mean Sq F value  Pr(>F)   
    Group2        2  1179646  589823   5.112 0.00766 **
    Residuals   102 11769635  115389                   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_mini$AUC_neutral, Pupil_neutral_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_mini$AUC_neutral and Pupil_neutral_mini$Group2 

       Men   NC   
    NC 0.016 -    
    OC 0.020 0.905

    P value adjustment method: holm 

``` r
  Mean_diameter_neutral <- aov(Mean_diameter_neutral ~ Group2, data = Pupil_neutral_mini)
summary(Mean_diameter_neutral)
```

                 Df Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  0.357 0.17835   3.817 0.0252 *
    Residuals   102  4.766 0.04672                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_neutral_mini$Mean_diameter_neutral, Pupil_neutral_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_neutral_mini$Mean_diameter_neutral and Pupil_neutral_mini$Group2 

       Men   NC   
    NC 0.021 -    
    OC 0.281 0.252

    P value adjustment method: holm 

## Negative

``` r
## Only anti-androgenic OC-users

  LR_amplit_negative <- aov(LR_amplit_negative ~ Group2, data = Pupil_negative_anti)
summary(LR_amplit_negative)
```

                Df Sum Sq Mean Sq F value   Pr(>F)    
    Group2       2  0.969  0.4845   9.988 0.000115 ***
    Residuals   96  4.657  0.0485                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_anti$LR_amplit_negative, Pupil_negative_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_anti$LR_amplit_negative and Pupil_negative_anti$Group2 

       Men   NC   
    NC 1e-04 -    
    OC 0.012 0.270

    P value adjustment method: holm 

``` r
  LR_latency_negative <- aov(LR_latency_negative ~ Group2, data = Pupil_negative_anti)
summary(LR_latency_negative)
```

                Df  Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  368298  184149   2.735 0.0699 .
    Residuals   96 6463492   67328                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_anti$LR_latency_negative, Pupil_negative_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_anti$LR_latency_negative and Pupil_negative_anti$Group2 

       Men  NC  
    NC 0.86 -   
    OC 0.10 0.10

    P value adjustment method: holm 

``` r
  Slope_negative <- aov(Slope_negative ~ Group2, data = Pupil_negative_anti)
summary(Slope_negative)
```

                Df    Sum Sq   Mean Sq F value Pr(>F)
    Group2       2 4.530e-08 2.266e-08   1.945  0.149
    Residuals   96 1.119e-06 1.165e-08               

``` r
pairwise.t.test(Pupil_negative_anti$Slope_negative, Pupil_negative_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_anti$Slope_negative and Pupil_negative_anti$Group2 

       Men  NC  
    NC 0.16 -   
    OC 0.63 0.63

    P value adjustment method: holm 

``` r
  AUC_negative <- aov(AUC_negative ~ Group2, data = Pupil_negative_anti)
summary(AUC_negative)
```

                Df   Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  1192286  596143   2.813  0.065 .
    Residuals   96 20344045  211917                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_anti$AUC_negative, Pupil_negative_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_anti$AUC_negative and Pupil_negative_anti$Group2 

       Men  NC  
    NC 0.13 -   
    OC 0.81 0.13

    P value adjustment method: holm 

``` r
Mean_diameter_negative <- aov(Mean_diameter_negative ~ Group2, data = Pupil_negative_anti)
summary(Mean_diameter_negative)
```

                Df Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  0.353 0.17639   3.594 0.0313 *
    Residuals   96  4.712 0.04908                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_anti$Mean_diameter_negative, Pupil_negative_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_anti$Mean_diameter_negative and Pupil_negative_anti$Group2 

       Men   NC   
    NC 0.026 -    
    OC 0.363 0.363

    P value adjustment method: holm 

``` r
### Without mini pill users

  LR_amplit_negative <- aov(LR_amplit_negative ~ Group2, data = Pupil_negative_mini)
summary(LR_amplit_negative)
```

                 Df Sum Sq Mean Sq F value   Pr(>F)    
    Group2        2  0.995  0.4976   10.47 7.43e-05 ***
    Residuals   100  4.752  0.0475                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_mini$LR_amplit_negative, Pupil_negative_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_mini$LR_amplit_negative and Pupil_negative_mini$Group2 

       Men     NC    
    NC 8.1e-05 -     
    OC 0.0045  0.3173

    P value adjustment method: holm 

``` r
  LR_latency_negative <- aov(LR_latency_negative ~ Group2, data = Pupil_negative_mini)
summary(LR_latency_negative)
```

                 Df  Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  401224  200612   2.964 0.0562 .
    Residuals   100 6767915   67679                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_mini$LR_latency_negative, Pupil_negative_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_mini$LR_latency_negative and Pupil_negative_mini$Group2 

       Men   NC   
    NC 0.858 -    
    OC 0.086 0.086

    P value adjustment method: holm 

``` r
  Slope_negative <- aov(Slope_negative ~ Group2, data = Pupil_negative_mini)
summary(Slope_negative)
```

                 Df    Sum Sq   Mean Sq F value Pr(>F)
    Group2        2 4.540e-08 2.270e-08   1.959  0.146
    Residuals   100 1.159e-06 1.159e-08               

``` r
pairwise.t.test(Pupil_negative_mini$Slope_negative, Pupil_negative_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_mini$Slope_negative and Pupil_negative_mini$Group2 

       Men  NC  
    NC 0.15 -   
    OC 0.57 0.57

    P value adjustment method: holm 

``` r
  AUC_negative <- aov(AUC_negative ~ Group2, data = Pupil_negative_mini)
summary(AUC_negative)
```

                 Df   Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  1007481  503741   2.367  0.099 .
    Residuals   100 21278200  212782                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_mini$AUC_negative, Pupil_negative_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_mini$AUC_negative and Pupil_negative_mini$Group2 

       Men  NC  
    NC 0.14 -   
    OC 0.85 0.19

    P value adjustment method: holm 

``` r
Mean_diameter_negative <- aov(Mean_diameter_negative ~ Group2, data = Pupil_negative_mini)
summary(Mean_diameter_negative)
```

                 Df Sum Sq Mean Sq F value Pr(>F)  
    Group2        2  0.352 0.17599   3.723 0.0276 *
    Residuals   100  4.728 0.04728                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_negative_mini$Mean_diameter_negative, Pupil_negative_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_negative_mini$Mean_diameter_negative and Pupil_negative_mini$Group2 

       Men   NC   
    NC 0.023 -    
    OC 0.365 0.365

    P value adjustment method: holm 

## Erotic

``` r
## Only anti-androgenic OC-users

  LR_amplit_ero <- aov(LR_amplit_erotic ~ Group2, data = Pupil_erotic_anti)
summary(LR_amplit_ero)
```

                Df Sum Sq Mean Sq F value  Pr(>F)   
    Group2       2  0.764  0.3822   6.389 0.00252 **
    Residuals   92  5.504  0.0598                   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic_anti$LR_amplit_erotic, Pupil_erotic_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_anti$LR_amplit_erotic and Pupil_erotic_anti$Group2 

       Men    NC    
    NC 0.0018 -     
    OC 0.1125 0.1748

    P value adjustment method: holm 

``` r
  LR_latency_erotic <- aov(LR_latency_erotic ~ Group2, data = Pupil_erotic_anti)
summary(LR_latency_erotic)
```

                Df  Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  176203   88102   3.187 0.0459 *
    Residuals   92 2543301   27645                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic_anti$LR_latency_erotic, Pupil_erotic_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_anti$LR_latency_erotic and Pupil_erotic_anti$Group2 

       Men   NC   
    NC 0.261 -    
    OC 0.292 0.044

    P value adjustment method: holm 

``` r
  Slope_erotic <- aov(Slope_erotic ~ Group2, data = Pupil_erotic_anti)
summary(Slope_erotic)
```

                Df    Sum Sq   Mean Sq F value Pr(>F)
    Group2       2 7.790e-08 3.893e-08   2.289  0.107
    Residuals   92 1.564e-06 1.700e-08               

``` r
pairwise.t.test(Pupil_erotic_anti$Slope_erotic, Pupil_erotic_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_anti$Slope_erotic and Pupil_erotic_anti$Group2 

       Men  NC  
    NC 0.11 -   
    OC 0.50 0.50

    P value adjustment method: holm 

``` r
  AUC_erotic <- aov(AUC_erotic ~ Group2, data = Pupil_erotic_anti)
summary(AUC_erotic)
```

                Df   Sum Sq Mean Sq F value Pr(>F)
    Group2       2   305633  152816   0.682  0.508
    Residuals   92 20616566  224093               

``` r
pairwise.t.test(Pupil_erotic_anti$AUC_erotic, Pupil_erotic_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_anti$AUC_erotic and Pupil_erotic_anti$Group2 

       Men  NC  
    NC 0.77 -   
    OC 0.91 0.91

    P value adjustment method: holm 

``` r
  Mean_diameter_erotic <- aov(Mean_diameter_erotic ~ Group2, data = Pupil_erotic_anti)
summary(Mean_diameter_erotic)
```

                Df Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  0.392 0.19600   4.024 0.0211 *
    Residuals   92  4.482 0.04871                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic_anti$Mean_diameter_erotic, Pupil_erotic_anti$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_anti$Mean_diameter_erotic and Pupil_erotic_anti$Group2 

       Men  NC  
    NC 0.02 -   
    OC 0.42 0.16

    P value adjustment method: holm 

``` r
### Without mini pill users

  LR_amplit_ero <- aov(LR_amplit_erotic ~ Group2, data = Pupil_erotic_mini)
summary(LR_amplit_ero)
```

                Df Sum Sq Mean Sq F value  Pr(>F)   
    Group2       2  0.787  0.3935   6.676 0.00192 **
    Residuals   97  5.718  0.0589                   
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic_mini$LR_amplit_erotic, Pupil_erotic_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_mini$LR_amplit_erotic and Pupil_erotic_mini$Group2 

       Men    NC    
    NC 0.0016 -     
    OC 0.0401 0.2640

    P value adjustment method: holm 

``` r
  LR_latency_erotic <- aov(LR_latency_erotic ~ Group2, data = Pupil_erotic_mini)
summary(LR_latency_erotic)
```

                Df  Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  176225   88112   3.099 0.0496 *
    Residuals   97 2757866   28432                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic_mini$LR_latency_erotic, Pupil_erotic_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_mini$LR_latency_erotic and Pupil_erotic_mini$Group2 

       Men   NC   
    NC 0.271 -    
    OC 0.326 0.047

    P value adjustment method: holm 

``` r
  Slope_erotic <- aov(Slope_erotic ~ Group2, data = Pupil_erotic_mini)
summary(Slope_erotic)
```

                Df    Sum Sq   Mean Sq F value Pr(>F)
    Group2       2 7.720e-08 3.859e-08   2.259   0.11
    Residuals   97 1.657e-06 1.708e-08               

``` r
pairwise.t.test(Pupil_erotic_mini$Slope_erotic, Pupil_erotic_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_mini$Slope_erotic and Pupil_erotic_mini$Group2 

       Men  NC  
    NC 0.11 -   
    OC 0.58 0.58

    P value adjustment method: holm 

``` r
  AUC_erotic <- aov(AUC_erotic ~ Group2, data = Pupil_erotic_mini)
summary(AUC_erotic)
```

                Df   Sum Sq Mean Sq F value Pr(>F)
    Group2       2   943081  471541   2.109  0.127
    Residuals   97 21685965  223567               

``` r
pairwise.t.test(Pupil_erotic_mini$AUC_erotic, Pupil_erotic_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_mini$AUC_erotic and Pupil_erotic_mini$Group2 

       Men  NC  
    NC 0.22 -   
    OC 0.18 0.73

    P value adjustment method: holm 

``` r
  Mean_diameter_erotic <- aov(Mean_diameter_erotic ~ Group2, data = Pupil_erotic_mini)
summary(Mean_diameter_erotic)
```

                Df Sum Sq Mean Sq F value Pr(>F)  
    Group2       2  0.388 0.19388   4.179 0.0182 *
    Residuals   97  4.500 0.04639                 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pairwise.t.test(Pupil_erotic_mini$Mean_diameter_erotic, Pupil_erotic_mini$Group2, p.adjust.method = "holm")
```


        Pairwise comparisons using t tests with pooled SD 

    data:  Pupil_erotic_mini$Mean_diameter_erotic and Pupil_erotic_mini$Group2 

       Men   NC   
    NC 0.016 -    
    OC 0.331 0.151

    P value adjustment method: holm 
