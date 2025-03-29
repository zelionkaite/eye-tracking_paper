# Psychometric_properties
Zelionkaite

# Libraries

``` r
library(psych)
```

    Warning: paketas 'psych' buvo sukurtas pagal R versiją 4.4.3

``` r
load("Psicho_metric_data.RData")
```

## GAD

``` r
alpha(metrics[, c("GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("GAD1", "GAD2", "GAD3", "GAD4", "GAD5", 
        "GAD6", "GAD7")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.82      0.81    0.82      0.38 4.3 0.026 0.88 0.54     0.36

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.76  0.82  0.86
    Duhachek  0.77  0.82  0.87

     Reliability if an item is dropped:
         raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    GAD1      0.75      0.75    0.74      0.33 3.0    0.036 0.020  0.32
    GAD2      0.76      0.75    0.75      0.34 3.1    0.034 0.024  0.32
    GAD3      0.76      0.76    0.76      0.34 3.1    0.035 0.025  0.32
    GAD4      0.80      0.79    0.80      0.39 3.9    0.029 0.033  0.36
    GAD5      0.80      0.80    0.80      0.40 4.0    0.028 0.034  0.36
    GAD6      0.82      0.82    0.82      0.43 4.4    0.026 0.027  0.42
    GAD7      0.83      0.82    0.82      0.44 4.7    0.024 0.025  0.42

     Item statistics 
           n raw.r std.r r.cor r.drop mean   sd
    GAD1 108  0.85  0.83  0.85   0.76 1.35 0.82
    GAD2 108  0.82  0.82  0.82   0.73 0.80 0.79
    GAD3 108  0.82  0.80  0.79   0.71 1.16 0.91
    GAD4 106  0.65  0.65  0.56   0.51 1.04 0.78
    GAD5 108  0.60  0.63  0.53   0.48 0.34 0.60
    GAD6 107  0.56  0.55  0.42   0.38 0.96 0.79
    GAD7 108  0.50  0.51  0.37   0.32 0.50 0.74

    Non missing response frequency for each item
            0    1    2    3 miss
    GAD1 0.11 0.54 0.24 0.11 0.00
    GAD2 0.40 0.44 0.12 0.04 0.00
    GAD3 0.23 0.49 0.17 0.11 0.00
    GAD4 0.25 0.51 0.21 0.04 0.02
    GAD5 0.72 0.21 0.06 0.00 0.00
    GAD6 0.27 0.55 0.12 0.06 0.01
    GAD7 0.62 0.29 0.06 0.03 0.00

# PANAS

## PA

``` r
alpha(metrics[, c("PA1", "PA2", "PA3", "PA4", "PA5", "PA6", "PA7", "PA8", "PA9", "PA10", "PA11")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("PA1", "PA2", "PA3", "PA4", "PA5", "PA6", 
        "PA7", "PA8", "PA9", "PA10", "PA11")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.83      0.83    0.84      0.31 4.9 0.024  3.2 0.54      0.3

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.78  0.83  0.87
    Duhachek  0.78  0.83  0.88

     Reliability if an item is dropped:
         raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    PA1       0.82      0.82    0.83      0.32 4.7    0.026 0.014  0.32
    PA2       0.83      0.83    0.84      0.32 4.8    0.025 0.014  0.32
    PA3       0.80      0.81    0.81      0.30 4.2    0.028 0.011  0.28
    PA4       0.80      0.80    0.81      0.29 4.1    0.029 0.011  0.27
    PA5       0.82      0.82    0.83      0.32 4.6    0.026 0.015  0.29
    PA6       0.82      0.82    0.83      0.32 4.6    0.026 0.015  0.30
    PA7       0.81      0.81    0.82      0.30 4.4    0.027 0.013  0.29
    PA8       0.81      0.82    0.83      0.31 4.5    0.027 0.014  0.30
    PA9       0.80      0.80    0.81      0.29 4.1    0.028 0.012  0.28
    PA10      0.83      0.83    0.84      0.33 4.9    0.025 0.013  0.33
    PA11      0.81      0.81    0.82      0.30 4.2    0.028 0.014  0.27

     Item statistics 
           n raw.r std.r r.cor r.drop mean   sd
    PA1  108  0.52  0.54  0.48   0.42  3.9 0.76
    PA2  107  0.52  0.51  0.42   0.38  3.1 0.91
    PA3  108  0.69  0.69  0.67   0.60  3.2 0.93
    PA4  108  0.74  0.74  0.73   0.66  3.4 0.83
    PA5  108  0.57  0.55  0.48   0.44  2.4 1.04
    PA6  108  0.57  0.56  0.50   0.45  3.2 0.92
    PA7  108  0.63  0.64  0.60   0.53  3.0 0.88
    PA8  108  0.61  0.61  0.56   0.50  3.5 0.91
    PA9  108  0.73  0.73  0.71   0.64  3.2 0.88
    PA10 108  0.46  0.46  0.37   0.33  3.3 0.85
    PA11 108  0.67  0.68  0.63   0.58  3.5 0.83

    Non missing response frequency for each item
            1    2    3    4    5 miss
    PA1  0.01 0.02 0.22 0.55 0.20 0.00
    PA2  0.04 0.23 0.39 0.30 0.04 0.01
    PA3  0.02 0.21 0.40 0.29 0.08 0.00
    PA4  0.00 0.17 0.35 0.43 0.06 0.00
    PA5  0.19 0.39 0.23 0.17 0.02 0.00
    PA6  0.03 0.17 0.41 0.32 0.07 0.00
    PA7  0.02 0.29 0.45 0.19 0.06 0.00
    PA8  0.00 0.14 0.35 0.36 0.15 0.00
    PA9  0.01 0.21 0.46 0.24 0.07 0.00
    PA10 0.01 0.14 0.46 0.31 0.08 0.00
    PA11 0.01 0.07 0.43 0.38 0.11 0.00

## NA

``` r
alpha(metrics[, c("NA1", "NA2", "NA3", "NA4", "NA5", "NA6", "NA7", "NA8", "NA9", "NA10", "NA11")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("NA1", "NA2", "NA3", "NA4", "NA5", "NA6", 
        "NA7", "NA8", "NA9", "NA10", "NA11")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.87      0.87    0.89      0.38 6.6 0.019  2.3 0.65     0.37

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.83  0.87  0.90
    Duhachek  0.83  0.87  0.91

     Reliability if an item is dropped:
         raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    NA1       0.86      0.86    0.88      0.37 6.0    0.020 0.016  0.36
    NA2       0.85      0.85    0.87      0.37 5.9    0.021 0.012  0.37
    NA3       0.87      0.86    0.88      0.39 6.3    0.019 0.016  0.37
    NA4       0.86      0.86    0.88      0.38 6.2    0.020 0.014  0.37
    NA5       0.87      0.87    0.89      0.40 6.7    0.019 0.013  0.39
    NA6       0.85      0.85    0.87      0.37 5.7    0.021 0.013  0.36
    NA7       0.86      0.86    0.88      0.38 6.2    0.020 0.015  0.37
    NA8       0.85      0.85    0.88      0.37 5.9    0.021 0.016  0.36
    NA9       0.85      0.85    0.87      0.37 5.8    0.021 0.016  0.36
    NA10      0.85      0.85    0.87      0.36 5.7    0.021 0.015  0.36
    NA11      0.86      0.86    0.87      0.38 6.1    0.020 0.013  0.37

     Item statistics 
           n raw.r std.r r.cor r.drop mean   sd
    NA1  108  0.66  0.67  0.62   0.58  2.2 0.94
    NA2  108  0.70  0.70  0.69   0.62  2.4 0.98
    NA3  107  0.59  0.59  0.52   0.48  2.1 1.08
    NA4  107  0.62  0.63  0.59   0.53  1.9 0.87
    NA5  108  0.48  0.50  0.41   0.38  2.0 0.82
    NA6  108  0.73  0.73  0.72   0.66  2.3 0.95
    NA7  108  0.63  0.63  0.58   0.53  2.7 1.04
    NA8  108  0.70  0.70  0.66   0.62  2.2 1.03
    NA9  108  0.72  0.71  0.68   0.63  2.9 1.10
    NA10 108  0.75  0.74  0.73   0.67  2.7 1.02
    NA11 108  0.65  0.65  0.62   0.56  2.0 1.00

    Non missing response frequency for each item
            1    2    3    4    5 miss
    NA1  0.25 0.43 0.23 0.08 0.01 0.00
    NA2  0.20 0.33 0.33 0.12 0.01 0.00
    NA3  0.35 0.40 0.14 0.07 0.05 0.01
    NA4  0.36 0.44 0.14 0.07 0.00 0.01
    NA5  0.30 0.46 0.21 0.02 0.01 0.00
    NA6  0.23 0.33 0.32 0.11 0.00 0.00
    NA7  0.13 0.30 0.36 0.17 0.05 0.00
    NA8  0.29 0.35 0.24 0.10 0.02 0.00
    NA9  0.09 0.30 0.32 0.20 0.08 0.00
    NA10 0.09 0.35 0.32 0.19 0.05 0.00
    NA11 0.41 0.29 0.23 0.06 0.01 0.00

# TAS

## Difficulty describing feelings

``` r
alpha(metrics[, c("TAS2D", "TAS4D", "TAS11D", "TAS12D", "TAS17D")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("TAS2D", "TAS4D", "TAS11D", "TAS12D", "TAS17D")])

      raw_alpha std.alpha G6(smc) average_r S/N  ase mean   sd median_r
          0.74      0.74    0.72      0.37 2.9 0.04  2.6 0.89     0.36

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.65  0.74  0.81
    Duhachek  0.66  0.74  0.82

     Reliability if an item is dropped:
           raw_alpha std.alpha G6(smc) average_r S/N alpha se  var.r med.r
    TAS2D       0.67      0.67    0.61      0.34 2.1    0.051 0.0022  0.36
    TAS4D       0.67      0.68    0.63      0.34 2.1    0.051 0.0059  0.36
    TAS11D      0.69      0.70    0.66      0.37 2.4    0.049 0.0127  0.37
    TAS12D      0.70      0.71    0.67      0.38 2.4    0.048 0.0138  0.36
    TAS17D      0.72      0.72    0.68      0.40 2.6    0.044 0.0106  0.37

     Item statistics 
             n raw.r std.r r.cor r.drop mean  sd
    TAS2D  108  0.72  0.75  0.68   0.55  2.7 1.2
    TAS4D  108  0.72  0.74  0.66   0.55  2.6 1.2
    TAS11D 108  0.69  0.69  0.57   0.49  2.6 1.2
    TAS12D 108  0.69  0.68  0.55   0.48  2.2 1.3
    TAS17D 108  0.69  0.65  0.51   0.44  3.0 1.5

    Non missing response frequency for each item
              1    2    3    4    5 miss
    TAS2D  0.15 0.41 0.10 0.30 0.05    0
    TAS4D  0.19 0.34 0.16 0.28 0.03    0
    TAS11D 0.21 0.36 0.12 0.24 0.06    0
    TAS12D 0.44 0.20 0.09 0.25 0.02    0
    TAS17D 0.21 0.22 0.08 0.27 0.21    0

## Difficulty identifying feelings

``` r
alpha(metrics[, c("TAS1I", "TAS3I", "TAS6I", "TAS7I", "TAS9I", "TAS13I", "TAS14I")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("TAS1I", "TAS3I", "TAS6I", "TAS7I", "TAS9I", 
        "TAS13I", "TAS14I")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.71      0.72    0.73      0.26 2.5 0.042  2.3 0.72     0.23

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.62  0.71  0.79
    Duhachek  0.63  0.71  0.79

     Reliability if an item is dropped:
           raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    TAS1I       0.66      0.66    0.67      0.25 2.0    0.051 0.017  0.23
    TAS3I       0.72      0.72    0.72      0.30 2.6    0.042 0.017  0.32
    TAS6I       0.66      0.67    0.66      0.25 2.0    0.050 0.016  0.21
    TAS7I       0.70      0.71    0.72      0.29 2.5    0.044 0.021  0.32
    TAS9I       0.67      0.67    0.66      0.25 2.0    0.049 0.012  0.23
    TAS13I      0.64      0.65    0.66      0.24 1.8    0.054 0.020  0.21
    TAS14I      0.68      0.69    0.69      0.27 2.2    0.047 0.023  0.27

     Item statistics 
             n raw.r std.r r.cor r.drop mean  sd
    TAS1I  108  0.66  0.67  0.60   0.50  2.4 1.1
    TAS3I  108  0.49  0.47  0.32   0.26  2.0 1.3
    TAS6I  108  0.64  0.66  0.60   0.49  2.4 1.1
    TAS7I  108  0.48  0.51  0.36   0.30  2.0 1.0
    TAS9I  108  0.65  0.64  0.60   0.46  2.9 1.3
    TAS13I 108  0.71  0.71  0.66   0.57  2.2 1.1
    TAS14I 107  0.62  0.60  0.50   0.41  2.4 1.4

    Non missing response frequency for each item
              1    2    3    4    5 miss
    TAS1I  0.21 0.47 0.08 0.20 0.03 0.00
    TAS3I  0.56 0.14 0.10 0.16 0.04 0.00
    TAS6I  0.23 0.40 0.17 0.19 0.01 0.00
    TAS7I  0.33 0.46 0.10 0.07 0.03 0.00
    TAS9I  0.19 0.24 0.10 0.43 0.04 0.00
    TAS13I 0.34 0.29 0.19 0.17 0.02 0.00
    TAS14I 0.37 0.26 0.10 0.15 0.11 0.01

## Externaly oriented thinking

``` r
alpha(metrics[, c("TAS5EX", "TAS8EX", "TAS10EX", "TAS15EX", "TAS16EX", "TAS18EX", "TAS19EX", "TAS20EX")])
```

    Warning in alpha(metrics[, c("TAS5EX", "TAS8EX", "TAS10EX", "TAS15EX", "TAS16EX", : Some items were negatively correlated with the first principal component and probably 
    should be reversed.  
    To do this, run the function again with the 'check.keys=TRUE' option

    Some items ( TAS20EX ) were negatively correlated with the first principal component and 
    probably should be reversed.  
    To do this, run the function again with the 'check.keys=TRUE' option


    Reliability analysis   
    Call: alpha(x = metrics[, c("TAS5EX", "TAS8EX", "TAS10EX", "TAS15EX", 
        "TAS16EX", "TAS18EX", "TAS19EX", "TAS20EX")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.56      0.55    0.59      0.13 1.2 0.063    2 0.52      0.1

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.42  0.56  0.67
    Duhachek  0.43  0.56  0.68

     Reliability if an item is dropped:
            raw_alpha std.alpha G6(smc) average_r  S/N alpha se var.r med.r
    TAS5EX       0.47      0.46    0.50     0.107 0.84    0.078 0.024 0.082
    TAS8EX       0.55      0.54    0.58     0.146 1.19    0.065 0.033 0.093
    TAS10EX      0.50      0.50    0.52     0.124 0.99    0.073 0.021 0.093
    TAS15EX      0.53      0.52    0.56     0.135 1.09    0.068 0.035 0.071
    TAS16EX      0.58      0.57    0.60     0.159 1.32    0.060 0.031 0.148
    TAS18EX      0.53      0.53    0.57     0.138 1.12    0.068 0.028 0.106
    TAS19EX      0.43      0.42    0.45     0.095 0.74    0.084 0.017 0.082
    TAS20EX      0.58      0.59    0.62     0.168 1.41    0.061 0.030 0.171

     Item statistics 
              n raw.r std.r r.cor r.drop mean   sd
    TAS5EX  107  0.63  0.64 0.597  0.453  1.8 0.99
    TAS8EX  108  0.44  0.43 0.271  0.190  2.1 1.08
    TAS10EX 108  0.54  0.55 0.491  0.338  1.8 1.00
    TAS15EX 108  0.50  0.49 0.347  0.263  2.3 1.08
    TAS16EX 108  0.37  0.36 0.170  0.104  2.4 1.12
    TAS18EX 108  0.47  0.47 0.344  0.244  1.7 1.01
    TAS19EX 108  0.71  0.70 0.721  0.520  2.3 1.15
    TAS20EX 108  0.28  0.31 0.093  0.066  1.6 0.89

    Non missing response frequency for each item
               1    2    3    4    5 miss
    TAS5EX  0.45 0.40 0.07 0.04 0.04 0.01
    TAS8EX  0.31 0.43 0.12 0.10 0.04 0.00
    TAS10EX 0.47 0.37 0.08 0.04 0.04 0.00
    TAS15EX 0.21 0.50 0.14 0.09 0.06 0.00
    TAS16EX 0.23 0.35 0.22 0.16 0.04 0.00
    TAS18EX 0.57 0.31 0.04 0.05 0.04 0.00
    TAS19EX 0.26 0.38 0.20 0.09 0.06 0.00
    TAS20EX 0.58 0.28 0.08 0.05 0.01 0.00

## Overall

``` r
alpha(metrics[, c("TAS1I", "TAS3I", "TAS6I", "TAS7I", "TAS9I", "TAS13I", "TAS14I", "TAS2D", "TAS4D", "TAS11D", "TAS12D", "TAS17D", "TAS5EX", "TAS8EX", "TAS10EX", "TAS15EX", "TAS16EX", "TAS18EX", "TAS19EX", "TAS20EX")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("TAS1I", "TAS3I", "TAS6I", "TAS7I", "TAS9I", 
        "TAS13I", "TAS14I", "TAS2D", "TAS4D", "TAS11D", "TAS12D", 
        "TAS17D", "TAS5EX", "TAS8EX", "TAS10EX", "TAS15EX", "TAS16EX", 
        "TAS18EX", "TAS19EX", "TAS20EX")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.81      0.81    0.86      0.17 4.2 0.025  2.3 0.54     0.18

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.76  0.81  0.86
    Duhachek  0.76  0.81  0.86

     Reliability if an item is dropped:
            raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    TAS1I        0.79      0.79    0.84      0.17 3.8    0.028 0.022  0.18
    TAS3I        0.82      0.81    0.86      0.19 4.3    0.025 0.023  0.19
    TAS6I        0.80      0.79    0.85      0.17 3.9    0.027 0.023  0.17
    TAS7I        0.81      0.81    0.85      0.18 4.1    0.026 0.024  0.18
    TAS9I        0.80      0.80    0.84      0.17 3.9    0.027 0.022  0.18
    TAS13I       0.79      0.79    0.84      0.16 3.7    0.028 0.022  0.17
    TAS14I       0.80      0.80    0.85      0.17 4.0    0.027 0.024  0.17
    TAS2D        0.80      0.79    0.84      0.17 3.9    0.027 0.021  0.17
    TAS4D        0.79      0.79    0.84      0.17 3.8    0.028 0.022  0.17
    TAS11D       0.79      0.79    0.84      0.17 3.8    0.028 0.022  0.17
    TAS12D       0.80      0.80    0.85      0.17 4.0    0.027 0.023  0.17
    TAS17D       0.80      0.80    0.85      0.17 4.0    0.027 0.023  0.17
    TAS5EX       0.81      0.81    0.85      0.18 4.1    0.026 0.023  0.18
    TAS8EX       0.80      0.80    0.85      0.17 4.0    0.027 0.024  0.17
    TAS10EX      0.81      0.81    0.85      0.18 4.2    0.026 0.022  0.19
    TAS15EX      0.81      0.80    0.85      0.18 4.0    0.026 0.024  0.18
    TAS16EX      0.82      0.82    0.86      0.19 4.5    0.024 0.022  0.19
    TAS18EX      0.82      0.81    0.86      0.19 4.4    0.025 0.022  0.19
    TAS19EX      0.80      0.80    0.85      0.17 3.9    0.027 0.023  0.18
    TAS20EX      0.81      0.81    0.86      0.19 4.3    0.025 0.023  0.19

     Item statistics 
              n raw.r std.r r.cor r.drop mean   sd
    TAS1I   108  0.64  0.64  0.64   0.58  2.4 1.11
    TAS3I   108  0.28  0.27  0.21   0.17  2.0 1.28
    TAS6I   108  0.56  0.56  0.55   0.49  2.4 1.07
    TAS7I   108  0.37  0.39  0.34   0.29  2.0 1.00
    TAS9I   108  0.57  0.56  0.55   0.49  2.9 1.26
    TAS13I  108  0.68  0.67  0.66   0.61  2.2 1.15
    TAS14I  107  0.52  0.50  0.47   0.41  2.4 1.40
    TAS2D   108  0.57  0.57  0.57   0.49  2.7 1.18
    TAS4D   108  0.64  0.64  0.63   0.57  2.6 1.17
    TAS11D  108  0.63  0.61  0.59   0.55  2.6 1.25
    TAS12D  108  0.51  0.50  0.46   0.42  2.2 1.29
    TAS17D  108  0.53  0.50  0.47   0.42  3.0 1.49
    TAS5EX  107  0.36  0.38  0.35   0.27  1.8 0.99
    TAS8EX  108  0.47  0.48  0.43   0.39  2.1 1.08
    TAS10EX 108  0.35  0.37  0.33   0.26  1.8 1.00
    TAS15EX 108  0.45  0.45  0.40   0.36  2.3 1.08
    TAS16EX 108  0.17  0.18  0.11   0.07  2.4 1.12
    TAS18EX 108  0.21  0.24  0.17   0.12  1.7 1.01
    TAS19EX 108  0.52  0.53  0.52   0.44  2.3 1.15
    TAS20EX 108  0.24  0.26  0.19   0.16  1.6 0.89

    Non missing response frequency for each item
               1    2    3    4    5 miss
    TAS1I   0.21 0.47 0.08 0.20 0.03 0.00
    TAS3I   0.56 0.14 0.10 0.16 0.04 0.00
    TAS6I   0.23 0.40 0.17 0.19 0.01 0.00
    TAS7I   0.33 0.46 0.10 0.07 0.03 0.00
    TAS9I   0.19 0.24 0.10 0.43 0.04 0.00
    TAS13I  0.34 0.29 0.19 0.17 0.02 0.00
    TAS14I  0.37 0.26 0.10 0.15 0.11 0.01
    TAS2D   0.15 0.41 0.10 0.30 0.05 0.00
    TAS4D   0.19 0.34 0.16 0.28 0.03 0.00
    TAS11D  0.21 0.36 0.12 0.24 0.06 0.00
    TAS12D  0.44 0.20 0.09 0.25 0.02 0.00
    TAS17D  0.21 0.22 0.08 0.27 0.21 0.00
    TAS5EX  0.45 0.40 0.07 0.04 0.04 0.01
    TAS8EX  0.31 0.43 0.12 0.10 0.04 0.00
    TAS10EX 0.47 0.37 0.08 0.04 0.04 0.00
    TAS15EX 0.21 0.50 0.14 0.09 0.06 0.00
    TAS16EX 0.23 0.35 0.22 0.16 0.04 0.00
    TAS18EX 0.57 0.31 0.04 0.05 0.04 0.00
    TAS19EX 0.26 0.38 0.20 0.09 0.06 0.00
    TAS20EX 0.58 0.28 0.08 0.05 0.01 0.00

# HEXACO

## Honesty

``` r
alpha(metrics[, c("H6", "H12",  "H18",  "H24",  "H30", "H36", "H42",    "H48", "H54", "H60")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("H6", "H12", "H18", "H24", "H30", "H36", 
        "H42", "H48", "H54", "H60")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.74      0.74    0.78      0.23 2.9 0.037  3.5 0.62      0.2

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.66  0.74  0.81
    Duhachek  0.67  0.74  0.82

     Reliability if an item is dropped:
        raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    H6       0.72      0.72    0.75      0.22 2.6    0.041 0.015  0.20
    H12      0.72      0.73    0.74      0.23 2.7    0.040 0.009  0.21
    H18      0.73      0.73    0.75      0.23 2.6    0.040 0.013  0.19
    H24      0.73      0.73    0.77      0.23 2.7    0.039 0.015  0.20
    H30      0.72      0.72    0.75      0.22 2.5    0.041 0.014  0.19
    H36      0.72      0.72    0.76      0.22 2.6    0.041 0.015  0.19
    H42      0.72      0.72    0.75      0.22 2.6    0.040 0.013  0.20
    H48      0.73      0.73    0.77      0.23 2.7    0.040 0.016  0.20
    H54      0.72      0.72    0.75      0.22 2.6    0.041 0.015  0.19
    H60      0.71      0.72    0.73      0.22 2.6    0.042 0.010  0.20

     Item statistics 
          n raw.r std.r r.cor r.drop mean  sd
    H6  107  0.56  0.56  0.49   0.42  3.6 1.1
    H12 107  0.59  0.54  0.51   0.41  3.1 1.4
    H18 107  0.52  0.54  0.47   0.38  3.0 1.0
    H24 107  0.48  0.50  0.41   0.34  3.7 1.0
    H30 107  0.57  0.58  0.52   0.43  3.7 1.1
    H36 108  0.57  0.56  0.48   0.42  3.4 1.2
    H42 108  0.55  0.56  0.51   0.41  3.4 1.1
    H48 108  0.50  0.52  0.43   0.36  3.5 1.0
    H54 108  0.55  0.57  0.50   0.42  3.8 1.0
    H60 108  0.60  0.57  0.54   0.45  3.7 1.2

    Non missing response frequency for each item
           1    2    3    4    5 miss
    H6  0.03 0.16 0.22 0.33 0.26 0.01
    H12 0.19 0.19 0.14 0.26 0.22 0.01
    H18 0.06 0.32 0.31 0.24 0.07 0.01
    H24 0.02 0.12 0.26 0.37 0.22 0.01
    H30 0.07 0.09 0.12 0.50 0.22 0.01
    H36 0.06 0.19 0.26 0.31 0.19 0.00
    H42 0.05 0.16 0.29 0.32 0.19 0.00
    H48 0.02 0.19 0.27 0.37 0.16 0.00
    H54 0.04 0.09 0.17 0.46 0.24 0.00
    H60 0.03 0.20 0.15 0.27 0.35 0.00

## Emotionality

``` r
alpha(metrics[, c("E5", "E11",  "E17",  "E23",  "E29",  "E35",  "E41",  "E47",  "E53",  "E59"   )])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("E5", "E11", "E17", "E23", "E29", "E35", 
        "E41", "E47", "E53", "E59")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.76      0.77    0.79      0.25 3.3 0.034  3.1 0.63     0.22

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.69  0.76  0.83
    Duhachek  0.70  0.76  0.83

     Reliability if an item is dropped:
        raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    E5       0.76      0.77    0.78      0.27 3.3    0.034 0.014  0.27
    E11      0.75      0.75    0.76      0.25 3.0    0.037 0.012  0.24
    E17      0.76      0.76    0.77      0.26 3.2    0.034 0.011  0.28
    E23      0.73      0.74    0.76      0.24 2.8    0.039 0.015  0.22
    E29      0.73      0.74    0.76      0.24 2.8    0.039 0.016  0.20
    E35      0.73      0.73    0.74      0.23 2.7    0.040 0.012  0.21
    E41      0.74      0.74    0.75      0.24 2.9    0.038 0.013  0.24
    E47      0.75      0.75    0.77      0.25 3.0    0.037 0.017  0.24
    E53      0.75      0.76    0.78      0.26 3.1    0.036 0.016  0.26
    E59      0.74      0.74    0.76      0.24 2.9    0.038 0.015  0.21

     Item statistics 
          n raw.r std.r r.cor r.drop mean  sd
    E5  107  0.45  0.44  0.34   0.29  2.4 1.1
    E11 107  0.55  0.56  0.50   0.41  3.8 1.1
    E17 106  0.48  0.47  0.39   0.31  3.4 1.2
    E23 107  0.63  0.63  0.58   0.50  2.8 1.2
    E29 107  0.64  0.64  0.59   0.52  3.0 1.1
    E35 108  0.68  0.69  0.67   0.57  3.8 1.0
    E41 108  0.60  0.60  0.56   0.47  2.6 1.1
    E47 108  0.56  0.55  0.46   0.41  2.9 1.2
    E53 108  0.51  0.52  0.42   0.37  3.3 1.0
    E59 108  0.59  0.60  0.54   0.47  3.3 1.1

    Non missing response frequency for each item
           1    2    3    4    5 miss
    E5  0.25 0.36 0.20 0.14 0.05 0.01
    E11 0.03 0.14 0.11 0.42 0.30 0.01
    E17 0.08 0.18 0.19 0.37 0.19 0.02
    E23 0.13 0.33 0.25 0.21 0.07 0.01
    E29 0.05 0.35 0.21 0.32 0.08 0.01
    E35 0.03 0.08 0.21 0.44 0.24 0.00
    E41 0.11 0.43 0.19 0.24 0.03 0.00
    E47 0.12 0.27 0.25 0.27 0.09 0.00
    E53 0.03 0.23 0.26 0.36 0.12 0.00
    E59 0.06 0.16 0.35 0.28 0.16 0.00

## Extroversion

``` r
alpha(metrics[, c("Ex4",    "Ex10", "Ex16", "Ex22", "Ex28", "Ex34", "Ex40", "Ex46", "Ex52", "Ex58")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("Ex4", "Ex10", "Ex16", "Ex22", "Ex28", 
        "Ex34", "Ex40", "Ex46", "Ex52", "Ex58")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.85      0.85    0.87      0.37 5.9 0.021    3 0.72     0.35

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.81  0.85  0.89
    Duhachek  0.81  0.85  0.89

     Reliability if an item is dropped:
         raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    Ex4       0.84      0.84    0.86      0.37 5.4    0.023 0.013  0.36
    Ex10      0.84      0.84    0.85      0.37 5.2    0.024 0.014  0.35
    Ex16      0.85      0.85    0.86      0.38 5.6    0.022 0.013  0.36
    Ex22      0.82      0.83    0.83      0.35 4.8    0.025 0.010  0.33
    Ex28      0.84      0.84    0.86      0.37 5.4    0.023 0.015  0.36
    Ex34      0.84      0.84    0.86      0.38 5.4    0.023 0.014  0.35
    Ex40      0.84      0.84    0.85      0.37 5.3    0.023 0.013  0.35
    Ex46      0.83      0.83    0.84      0.36 5.0    0.025 0.012  0.34
    Ex52      0.85      0.85    0.86      0.38 5.5    0.022 0.012  0.36
    Ex58      0.83      0.84    0.85      0.36 5.1    0.024 0.015  0.34

     Item statistics 
           n raw.r std.r r.cor r.drop mean  sd
    Ex4  107  0.62  0.63  0.57   0.52  3.4 1.0
    Ex10 107  0.68  0.66  0.62   0.56  3.3 1.3
    Ex16 107  0.58  0.58  0.51   0.47  2.6 1.1
    Ex22 107  0.78  0.79  0.79   0.72  3.4 1.0
    Ex28 107  0.63  0.63  0.57   0.53  3.0 1.1
    Ex34 108  0.62  0.62  0.56   0.51  3.0 1.1
    Ex40 108  0.65  0.65  0.60   0.54  2.8 1.2
    Ex46 108  0.72  0.73  0.71   0.64  3.1 1.1
    Ex52 108  0.60  0.59  0.53   0.47  2.9 1.2
    Ex58 108  0.70  0.70  0.66   0.61  2.7 1.1

    Non missing response frequency for each item
            1    2    3    4    5 miss
    Ex4  0.03 0.20 0.26 0.40 0.11 0.01
    Ex10 0.12 0.19 0.18 0.33 0.19 0.01
    Ex16 0.14 0.39 0.28 0.13 0.06 0.01
    Ex22 0.05 0.16 0.28 0.42 0.09 0.01
    Ex28 0.07 0.25 0.35 0.25 0.08 0.01
    Ex34 0.10 0.19 0.31 0.33 0.06 0.00
    Ex40 0.11 0.38 0.21 0.19 0.10 0.00
    Ex46 0.04 0.32 0.23 0.33 0.07 0.00
    Ex52 0.07 0.40 0.22 0.18 0.13 0.00
    Ex58 0.15 0.30 0.28 0.26 0.02 0.00

## Agreeableness

``` r
alpha(metrics[, c("A3", "A9",   "A15",  "A21",  "A27",  "A33",  "A39",  "A45", "A51",   "A57")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("A3", "A9", "A15", "A21", "A27", "A33", 
        "A39", "A45", "A51", "A57")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.77      0.77    0.81      0.26 3.4 0.033  3.2 0.63     0.25

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.70  0.77  0.83
    Duhachek  0.71  0.77  0.84

     Reliability if an item is dropped:
        raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    A3       0.76      0.76    0.79      0.26 3.1    0.035 0.018  0.25
    A9       0.73      0.73    0.77      0.23 2.8    0.040 0.020  0.23
    A15      0.76      0.77    0.80      0.27 3.3    0.034 0.021  0.27
    A21      0.75      0.75    0.78      0.25 3.1    0.036 0.021  0.25
    A27      0.75      0.75    0.79      0.25 3.0    0.037 0.020  0.25
    A33      0.73      0.73    0.77      0.24 2.8    0.039 0.021  0.24
    A39      0.75      0.76    0.79      0.26 3.2    0.035 0.022  0.25
    A45      0.76      0.76    0.78      0.26 3.2    0.035 0.017  0.25
    A51      0.77      0.78    0.79      0.28 3.5    0.033 0.015  0.26
    A57      0.75      0.76    0.79      0.26 3.1    0.036 0.023  0.26

     Item statistics 
          n raw.r std.r r.cor r.drop mean   sd
    A3  107  0.55  0.55  0.50   0.41  3.1 1.15
    A9  107  0.72  0.71  0.68   0.60  3.4 1.20
    A15 107  0.53  0.50  0.42   0.36  3.1 1.26
    A21 107  0.58  0.58  0.53   0.45  3.8 1.03
    A27 107  0.60  0.61  0.55   0.48  2.8 1.05
    A33 108  0.69  0.70  0.67   0.60  3.5 0.93
    A39 108  0.54  0.55  0.48   0.41  3.1 1.01
    A45 108  0.55  0.55  0.50   0.40  3.2 1.11
    A51 108  0.41  0.42  0.34   0.25  2.8 1.07
    A57 108  0.58  0.57  0.50   0.44  2.7 1.08

    Non missing response frequency for each item
           1    2    3    4    5 miss
    A3  0.09 0.22 0.24 0.35 0.09 0.01
    A9  0.07 0.19 0.18 0.37 0.19 0.01
    A15 0.13 0.21 0.24 0.28 0.14 0.01
    A21 0.01 0.13 0.21 0.36 0.28 0.01
    A27 0.10 0.32 0.29 0.25 0.04 0.01
    A33 0.03 0.11 0.29 0.46 0.11 0.00
    A39 0.05 0.24 0.31 0.34 0.06 0.00
    A45 0.06 0.24 0.28 0.31 0.11 0.00
    A51 0.10 0.35 0.22 0.30 0.03 0.00
    A57 0.15 0.33 0.26 0.23 0.03 0.00

# Conscientiousness

``` r
alpha(metrics[, c("C2", "C8",   "C14",  "C20",  "C26",  "C32",  "C38",  "C44",  "C50",  "C56")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("C2", "C8", "C14", "C20", "C26", "C32", 
        "C38", "C44", "C50", "C56")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.75      0.76    0.79      0.24 3.1 0.036  3.4 0.59     0.23

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.67  0.75  0.81
    Duhachek  0.68  0.75  0.82

     Reliability if an item is dropped:
        raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    C2       0.72      0.73    0.75      0.23 2.7    0.041 0.015  0.23
    C8       0.74      0.75    0.78      0.25 3.0    0.039 0.014  0.24
    C14      0.73      0.74    0.77      0.24 2.8    0.040 0.014  0.22
    C20      0.73      0.73    0.77      0.24 2.8    0.040 0.018  0.22
    C26      0.74      0.74    0.77      0.24 2.9    0.038 0.016  0.24
    C32      0.74      0.75    0.78      0.25 3.0    0.039 0.017  0.23
    C38      0.72      0.73    0.76      0.23 2.7    0.041 0.016  0.22
    C44      0.72      0.73    0.76      0.23 2.6    0.041 0.017  0.23
    C50      0.73      0.74    0.77      0.24 2.9    0.038 0.015  0.23
    C56      0.73      0.74    0.77      0.24 2.8    0.040 0.016  0.22

     Item statistics 
          n raw.r std.r r.cor r.drop mean   sd
    C2  107  0.62  0.61  0.57   0.49  3.7 1.05
    C8  107  0.49  0.49  0.41   0.35  3.6 0.98
    C14 107  0.54  0.56  0.50   0.41  3.9 0.90
    C20 107  0.55  0.58  0.51   0.42  3.5 0.98
    C26 107  0.56  0.52  0.45   0.36  3.0 1.40
    C32 108  0.50  0.49  0.41   0.35  3.2 1.03
    C38 108  0.61  0.62  0.58   0.49  3.8 0.94
    C44 108  0.62  0.63  0.59   0.50  3.4 0.96
    C50 108  0.57  0.54  0.48   0.38  3.0 1.32
    C56 108  0.54  0.56  0.49   0.41  3.1 0.99

    Non missing response frequency for each item
           1    2    3    4    5 miss
    C2  0.04 0.09 0.21 0.41 0.24 0.01
    C8  0.02 0.12 0.29 0.39 0.18 0.01
    C14 0.02 0.07 0.17 0.52 0.22 0.01
    C20 0.00 0.21 0.24 0.40 0.15 0.01
    C26 0.18 0.27 0.14 0.22 0.19 0.01
    C32 0.03 0.27 0.27 0.34 0.09 0.00
    C38 0.00 0.13 0.17 0.47 0.23 0.00
    C44 0.03 0.15 0.29 0.44 0.10 0.00
    C50 0.19 0.20 0.19 0.31 0.12 0.00
    C56 0.04 0.27 0.30 0.34 0.06 0.00

## Openness to experience

``` r
alpha(metrics[, c("O1", "O7",   "O13",  "O19",  "O25",  "O31",  "O37",  "O43",  "O49",  "O55")])
```


    Reliability analysis   
    Call: alpha(x = metrics[, c("O1", "O7", "O13", "O19", "O25", "O31", 
        "O37", "O43", "O49", "O55")])

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.73      0.73    0.76      0.21 2.7 0.039  3.7 0.61     0.21

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.65  0.73   0.8
    Duhachek  0.65  0.73   0.8

     Reliability if an item is dropped:
        raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    O1       0.70      0.70    0.72      0.21 2.4    0.043 0.013  0.18
    O7       0.73      0.72    0.73      0.23 2.6    0.039 0.011  0.21
    O13      0.70      0.70    0.73      0.21 2.3    0.044 0.015  0.19
    O19      0.72      0.73    0.75      0.23 2.7    0.040 0.015  0.21
    O25      0.69      0.69    0.71      0.20 2.2    0.045 0.013  0.18
    O31      0.71      0.71    0.73      0.22 2.5    0.041 0.015  0.21
    O37      0.69      0.70    0.71      0.20 2.3    0.044 0.013  0.18
    O43      0.72      0.72    0.74      0.22 2.6    0.041 0.016  0.21
    O49      0.70      0.71    0.73      0.21 2.4    0.043 0.013  0.21
    O55      0.71      0.71    0.74      0.22 2.5    0.041 0.016  0.21

     Item statistics 
          n raw.r std.r r.cor r.drop mean   sd
    O1  107  0.58  0.58  0.53   0.42  3.7 1.18
    O7  107  0.47  0.46  0.39   0.28  3.4 1.26
    O13 107  0.61  0.58  0.52   0.45  3.8 1.27
    O19 107  0.42  0.45  0.34   0.27  3.4 0.99
    O25 107  0.65  0.65  0.62   0.52  4.0 1.09
    O31 108  0.49  0.52  0.44   0.37  4.2 0.90
    O37 108  0.64  0.61  0.57   0.49  3.5 1.16
    O43 108  0.44  0.49  0.39   0.33  3.9 0.79
    O49 108  0.59  0.55  0.49   0.42  3.6 1.27
    O55 108  0.52  0.52  0.43   0.36  3.7 1.20

    Non missing response frequency for each item
           1    2    3    4    5 miss
    O1  0.06 0.13 0.16 0.36 0.29 0.01
    O7  0.07 0.20 0.24 0.23 0.25 0.01
    O13 0.07 0.10 0.13 0.29 0.40 0.01
    O19 0.02 0.15 0.41 0.26 0.16 0.01
    O25 0.04 0.07 0.14 0.34 0.41 0.01
    O31 0.02 0.05 0.06 0.44 0.44 0.00
    O37 0.05 0.18 0.23 0.31 0.23 0.00
    O43 0.00 0.05 0.21 0.52 0.22 0.00
    O49 0.09 0.15 0.12 0.39 0.25 0.00
    O55 0.07 0.10 0.15 0.39 0.29 0.00
