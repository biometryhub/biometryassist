# example 1 works

    Analysis of Variance Table
    
    Response: RL
              Df Sum Sq Mean Sq F value          Pr(>F)    
    trt        3 190.79  63.598  77.675 0.0000000009414 ***
    Residuals 16  13.10   0.819                            
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

      trt predicted.value std.error df groups   ci   low    up
    1  20            9.96       0.4 16      a 0.71  9.26 10.67
    2   1           12.26       0.4 16      b 0.71 11.55 12.96
    3   5           16.14       0.4 16      c 0.71 15.44 16.85
    4  10           17.77       0.4 16      c 0.71 17.07 18.48

# example 2 works

    Analysis of Variance Table
    
    Response: TuberLengthGrowth
              Df Sum Sq Mean Sq F value    Pr(>F)    
    trt        6 436.01  72.668  54.989 < 2.2e-16 ***
    Residuals 77 101.76   1.322                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

      trt predicted.value std.error df groups     ci     low      up
    1  T3         11.1525    0.3319 77      a 0.5525 10.6000 11.7050
    2  T5         12.4542    0.3319 77      a 0.5525 11.9017 13.0067
    3  T6         14.0250    0.3319 77      b 0.5525 13.4725 14.5775
    4  T4         15.1008    0.3319 77     bc 0.5525 14.5483 15.6533
    5  T2         16.1117    0.3319 77     cd 0.5525 15.5592 16.6642
    6  T7         17.2400    0.3319 77     de 0.5525 16.6875 17.7925
    7  T1         17.8292    0.3319 77      e 0.5525 17.2767 18.3817

# example 3 works

    Analysis of Variance Table
    
    Response: Yield
              Df Sum Sq Mean Sq F value   Pr(>F)   
    Block      4 29.191  7.2977  3.7085 0.034525 * 
    Variety    3 36.527 12.1756  6.1873 0.008747 **
    Residuals 12 23.614  1.9678                    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

        Variety predicted.value std.error df groups   ci  low   up
    1 Parafield            1.68      0.63 12      a 1.12 0.56 2.80
    2     Kaspa            2.68      0.63 12     ab 1.12 1.56 3.79
    3    Yarrum            4.72      0.63 12      b 1.12 3.61 5.84
    4    Excell            4.85      0.63 12      b 1.12 3.73 5.97

# example 4 works

    Analysis of Variance Table
    
    Response: DM
              Df Sum Sq Mean Sq F value   Pr(>F)   
    row        3  45760   15253  0.9928 0.457425   
    col        3  59232   19744  1.2851 0.361855   
    trt        3 613113  204371 13.3020 0.004638 **
    Residuals  6  92184   15364                    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

      trt predicted.value std.error df groups     ci     low      up
    1  S4         1707.94     61.98  6      a 120.43 1587.51 1828.37
    2  S2         1802.70     61.98  6     ab 120.43 1682.27 1923.13
    3  S1         2053.73     61.98  6     bc 120.43 1933.30 2174.16
    4  S3         2200.08     61.98  6      c 120.43 2079.66 2320.51

# exercise 1 works

    Analysis of Variance Table
    
    Response: Yield
              Df  Sum Sq  Mean Sq F value    Pr(>F)    
    Variety   11 2.14254 0.194777  4.6796 0.0007707 ***
    Residuals 24 0.99893 0.041622                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

        Variety predicted.value std.error df groups      ci     low      up
    1      Lang         1.97333   0.11779 24      a 0.20152 1.77181 2.17486
    2  Drysdale         2.13000   0.11779 24      a 0.20152 1.92848 2.33152
    3     Wylah         2.13000   0.11779 24      a 0.20152 1.92848 2.33152
    4    Baxter         2.14000   0.11779 24      a 0.20152 1.93848 2.34152
    5      Janz         2.19333   0.11779 24     ab 0.20152 1.99181 2.39486
    6    Endure         2.24000   0.11779 24     ab 0.20152 2.03848 2.44152
    7     Orion         2.27000   0.11779 24     ab 0.20152 2.06848 2.47152
    8     Zippy         2.28333   0.11779 24     ab 0.20152 2.08181 2.48486
    9   Fortune         2.52667   0.11779 24     ab 0.20152 2.32514 2.72819
    10  Caryina         2.54000   0.11779 24     ab 0.20152 2.33848 2.74152
    11  Pugsley         2.75000   0.11779 24      b 0.20152 2.54848 2.95152
    12   Arrino         2.75333   0.11779 24      b 0.20152 2.55181 2.95486

# exercise 2 works

    Analysis of Variance Table
    
    Response: Time
              Df Sum Sq Mean Sq F value   Pr(>F)   
    Treatment  5 4.2352 0.84704  6.4554 0.001331 **
    Residuals 18 2.3619 0.13122                    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      data.frame(lapply(pred2e.out, function(y) if (is.numeric(y)) round(y, 1) else y))
    Output
        Treatment predicted.value std.error df groups  ci low  up
      1        KC             2.1       0.2 18      a 0.3 1.8 2.4
      2        PE             2.2       0.2 18      a 0.3 1.9 2.5
      3        HL             2.6       0.2 18     ab 0.3 2.3 2.9
      4        CN             2.8       0.2 18     ab 0.3 2.5 3.1
      5        HE             2.8       0.2 18     ab 0.3 2.5 3.1
      6        CP             3.4       0.2 18      b 0.3 3.0 3.7

# exercise 3 works

    Analysis of Variance Table
    
    Response: AverageFruitSize
              Df  Sum Sq Mean Sq F value    Pr(>F)    
    Replicate  4  44.969 11.2421  12.696 1.092e-05 ***
    Variety    6 134.623 22.4371  25.339 2.868e-09 ***
    Residuals 24  21.251  0.8855                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

            Variety predicted.value std.error df groups   ci  low   up
    1 CarolinaCross            2.84      0.42 24      a 0.72 2.12 3.56
    2       Pharoah            2.86      0.42 24      a 0.72 2.14 3.58
    3       Phantom            3.08      0.42 24     ab 0.72 2.36 3.80
    4      Hercules            4.70      0.42 24     ab 0.72 3.98 5.42
    5  Melitopolski            4.78      0.42 24      b 0.72 4.06 5.50
    6     Orangeglo            4.96      0.42 24      b 0.72 4.24 5.68
    7      Sudanese            8.88      0.42 24      c 0.72 8.16 9.60

# exercise 4 works

    Analysis of Variance Table
    
    Response: Yield
                Df  Sum Sq Mean Sq F value  Pr(>F)  
    Block        3 1.94436 0.64812  4.9021 0.01434 *
    SeedingRate  5 0.87353 0.17471  1.3214 0.30758  
    Residuals   15 1.98317 0.13221                  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# exercise 5 works

    Analysis of Variance Table
    
    Response: EarInfect
              Df Sum Sq Mean Sq F value    Pr(>F)    
    row        1   2.71   2.710  0.2773    0.6049    
    col        1   2.67   2.668  0.2730    0.6077    
    Treatment  4 826.56 206.640 21.1476 1.327e-06 ***
    Residuals 18 175.88   9.771                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

      Treatment predicted.value std.error df groups   ci   low    up
    1      Root           31.61       1.4 18      a 2.42 29.18 34.03
    2      Seed           35.98       1.4 18     ab 2.42 33.55 38.40
    3     Stalk           38.95       1.4 18     bc 2.42 36.53 41.38
    4    Damage           43.52       1.4 18     cd 2.42 41.09 45.94
    5      Silk           48.12       1.4 18      d 2.42 45.69 50.54

# exercise 6 works

    Analysis of Variance Table
    
    Response: SugarYield
              Df  Sum Sq Mean Sq F value  Pr(>F)  
    row        1  11.666  11.666  1.1452 0.30971  
    col        1  21.084  21.084  2.0696 0.18081  
    Treatment  3 172.965  57.655  5.6594 0.01574 *
    Residuals 10 101.874  10.187                  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

      Treatment predicted.value std.error df groups   ci   low    up
    1        T4           16.01       1.6 10      a 2.89 13.12 18.90
    2        T8           17.51       1.6 10     ab 2.89 14.62 20.40
    3       T12           21.40       1.6 10     ab 2.89 18.51 24.29
    4        T0           24.39       1.6 10      b 2.89 21.50 27.28

