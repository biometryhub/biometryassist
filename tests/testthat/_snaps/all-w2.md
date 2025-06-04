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
    1  20            9.96       0.4 16      a 0.86  9.11 10.82
    2   1           12.26       0.4 16      b 0.86 11.40 13.12
    3   5           16.14       0.4 16      c 0.86 15.29 17.00
    4  10           17.77       0.4 16      c 0.86 16.92 18.63

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
    1  T3         11.1525    0.3319 77      a 0.6608 10.4917 11.8133
    2  T5         12.4542    0.3319 77      a 0.6608 11.7934 13.1150
    3  T6         14.0250    0.3319 77      b 0.6608 13.3642 14.6858
    4  T4         15.1008    0.3319 77     bc 0.6608 14.4400 15.7616
    5  T2         16.1117    0.3319 77     cd 0.6608 15.4509 16.7725
    6  T7         17.2400    0.3319 77     de 0.6608 16.5792 17.9008
    7  T1         17.8292    0.3319 77      e 0.6608 17.1684 18.4900

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
    1 Parafield            1.68      0.63 12      a 1.37 0.31 3.05
    2     Kaspa            2.68      0.63 12     ab 1.37 1.31 4.04
    3    Yarrum            4.72      0.63 12      b 1.37 3.36 6.09
    4    Excell            4.85      0.63 12      b 1.37 3.48 6.21

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
    1  S4         1707.94     61.98  6      a 151.65 1556.29 1859.59
    2  S2         1802.70     61.98  6     ab 151.65 1651.05 1954.35
    3  S1         2053.73     61.98  6     bc 151.65 1902.08 2205.38
    4  S3         2200.08     61.98  6      c 151.65 2048.44 2351.73

# example 3 LMM works

                Df denDF  F.inc          Pr
    (Intercept)  1     4 33.230 0.004494493
    Variety      3    12  6.187 0.008747377

---

        Variety predicted.value std.error groups   ci   low   up
    1 Parafield            1.68      0.81      a 1.77 -0.09 3.45
    2     Kaspa            2.68      0.81     ab 1.77  0.91 4.45
    3    Yarrum            4.72      0.81      b 1.77  2.95 6.49
    4    Excell            4.85      0.81      b 1.77  3.08 6.62

# example 4 LMM works

                Df denDF   F.inc           Pr
    (Intercept)  1     3 3053.00 1.305451e-05
    trt          3     9   13.33 1.162778e-03

---

      trt predicted.value std.error groups     ci     low      up
    1  S4         1707.94     64.09      a 144.99 1562.95 1852.93
    2  S2         1802.70     64.09     ab 144.99 1657.71 1947.68
    3  S1         2053.73     64.09     bc 144.99 1908.75 2198.72
    4  S3         2200.09     64.09      c 144.99 2055.10 2345.07

# example 5 works

                       Df denDF     F.inc           Pr
    (Intercept)         1     3 3029.0000 1.321435e-05
    Genotype           69   414    7.2010 0.000000e+00
    Fungicide           1     3   40.2700 7.914935e-03
    Genotype:Fungicide 69   414    0.9331 6.290102e-01

---

       Genotype predicted.value std.error    groups   ci  low   up
    1       G04            4.14      0.14         a 0.27 3.88 4.41
    2       G14            4.63      0.14        ab 0.27 4.36 4.90
    3       G20            4.80      0.14        bc 0.27 4.53 5.07
    4       G10            4.81      0.14       bcd 0.27 4.54 5.08
    5       G28            4.84      0.14      bcde 0.27 4.57 5.11
    6       G24            4.85      0.14     bcdef 0.27 4.59 5.12
    7       G01            4.91      0.14    bcdefg 0.27 4.64 5.18
    8       G05            4.94      0.14   bcdefgh 0.27 4.67 5.21
    9       G49            4.95      0.14   bcdefgh 0.27 4.68 5.22
    10      G34            5.01      0.14   bcdefgh 0.27 4.74 5.28
    11      G25            5.02      0.14   bcdefgh 0.27 4.75 5.29
    12      G30            5.04      0.14  bcdefghi 0.27 4.77 5.31
    13      G21            5.05      0.14  bcdefghi 0.27 4.78 5.32
    14      G02            5.06      0.14  bcdefghi 0.27 4.79 5.33
    15      G15            5.07      0.14  bcdefghi 0.27 4.80 5.34
    16      G41            5.09      0.14  bcdefghi 0.27 4.82 5.36
    17      G08            5.09      0.14  bcdefghi 0.27 4.82 5.36
    18      G07            5.10      0.14  bcdefghi 0.27 4.83 5.37
    19      G45            5.11      0.14  bcdefghi 0.27 4.84 5.38
    20      G06            5.13      0.14  bcdefghi 0.27 4.86 5.40
    21      G55            5.14      0.14  bcdefghi 0.27 4.87 5.41
    22      G32            5.16      0.14 bcdefghij 0.27 4.89 5.43
    23      G11            5.16      0.14 bcdefghij 0.27 4.90 5.43
    24      G39            5.18      0.14 bcdefghij 0.27 4.91 5.45
    25      G61            5.20      0.14 bcdefghij 0.27 4.93 5.47
    26      G53            5.21      0.14 bcdefghij 0.27 4.94 5.48
    27      G37            5.21      0.14 bcdefghij 0.27 4.94 5.48
    28      G26            5.21      0.14 bcdefghij 0.27 4.94 5.48
    29      G43            5.21      0.14 bcdefghij 0.27 4.95 5.48
    30      G35            5.22      0.14 bcdefghij 0.27 4.95 5.49
    31      G66            5.22      0.14 bcdefghij 0.27 4.95 5.49
    32      G16            5.23      0.14  cdefghij 0.27 4.96 5.50
    33      G68            5.25      0.14  cdefghij 0.27 4.98 5.52
    34      G46            5.25      0.14  cdefghij 0.27 4.98 5.52
    35      G29            5.26      0.14  cdefghij 0.27 4.99 5.53
    36      G47            5.27      0.14  cdefghij 0.27 5.00 5.54
    37      G44            5.29      0.14  cdefghij 0.27 5.02 5.56
    38      G23            5.29      0.14  cdefghij 0.27 5.02 5.56
    39      G56            5.29      0.14  cdefghij 0.27 5.03 5.56
    40      G58            5.31      0.14  cdefghij 0.27 5.04 5.58
    41      G57            5.31      0.14  cdefghij 0.27 5.05 5.58
    42      G63            5.32      0.14  cdefghij 0.27 5.05 5.59
    43      G62            5.32      0.14  cdefghij 0.27 5.05 5.59
    44      G54            5.33      0.14  cdefghij 0.27 5.06 5.60
    45      G51            5.36      0.14  cdefghij 0.27 5.09 5.63
    46      G67            5.37      0.14  cdefghij 0.27 5.10 5.64
    47      G22            5.37      0.14  cdefghij 0.27 5.10 5.64
    48      G42            5.37      0.14  cdefghij 0.27 5.10 5.64
    49      G59            5.37      0.14  cdefghij 0.27 5.10 5.64
    50      G65            5.38      0.14  cdefghij 0.27 5.11 5.65
    51      G69            5.38      0.14  cdefghij 0.27 5.11 5.65
    52      G38            5.38      0.14  cdefghij 0.27 5.11 5.65
    53      G12            5.39      0.14   defghij 0.27 5.12 5.66
    54      G31            5.41      0.14    efghij 0.27 5.14 5.68
    55      G50            5.42      0.14    efghij 0.27 5.15 5.69
    56      G17            5.42      0.14    efghij 0.27 5.16 5.69
    57      G52            5.43      0.14    efghij 0.27 5.16 5.70
    58      G64            5.44      0.14     fghij 0.27 5.17 5.71
    59      G13            5.45      0.14      ghij 0.27 5.18 5.72
    60      G70            5.45      0.14      ghij 0.27 5.18 5.72
    61      G09            5.46      0.14      ghij 0.27 5.19 5.73
    62      G60            5.48      0.14      ghij 0.27 5.21 5.75
    63      G27            5.49      0.14     ghijk 0.27 5.22 5.76
    64      G18            5.50      0.14     ghijk 0.27 5.23 5.77
    65      G36            5.50      0.14      hijk 0.27 5.24 5.77
    66      G48            5.51      0.14      hijk 0.27 5.24 5.78
    67      G40            5.53      0.14      hijk 0.27 5.26 5.80
    68      G33            5.61      0.14       ijk 0.27 5.34 5.88
    69      G19            5.73      0.14        jk 0.27 5.46 6.00
    70      G03            6.07      0.14         k 0.27 5.81 6.34

---

      Fungicide predicted.value std.error groups   ci  low   up
    1        F2            4.97       0.1      a 0.33 4.63 5.30
    2        F1            5.51       0.1      b 0.33 5.18 5.85

# example 6 works

                Df denDF   F.inc           Pr
    (Intercept)  1   6.9 188.100 2.807176e-06
    Treatment   20  27.6   2.963 4.353194e-03

---

                          component  std.error  z.ratio bound %ch
    Block              2.864157e-07         NA       NA     B  NA
    Column:Row!R       1.790098e-01 0.04842597 3.696565     P 0.0
    Column:Row!Row!cor 5.407771e-01 0.13654557 3.960414     U 0.1

---

            Treatment predicted.value std.error groups   ci  low   up
    1  Hoegrass_0.75L            0.92      0.20      a 0.42 0.50 1.34
    2   Wildcat_300mL            0.94      0.20      a 0.42 0.52 1.35
    3  Atlantis_300mL            0.94      0.21     ab 0.43 0.51 1.37
    4   Wildcat_350mL            0.94      0.20      a 0.42 0.52 1.36
    5      Topik_50mL            0.95      0.20      a 0.42 0.53 1.36
    6    Achieve_300g            0.96      0.21     ab 0.43 0.54 1.39
    7       Control_0            0.98      0.21      a 0.43 0.56 1.41
    8   Hoegrass_1.0L            1.06      0.20     ab 0.42 0.65 1.48
    9    Tristar_1.0L            1.07      0.20     ab 0.42 0.65 1.48
    10   Achieve_380g            1.08      0.20     ab 0.42 0.67 1.50
    11 Atlantis_330mL            1.18      0.20     ab 0.42 0.77 1.60
    12     Topik_85mL            1.22      0.21     ab 0.42 0.80 1.64
    13 MatavenL_2.25L            1.23      0.21     ab 0.42 0.81 1.65
    14  Hoegrass_1.2L            1.30      0.21     ab 0.42 0.87 1.72
    15     Topik_65mL            1.37      0.20     ab 0.42 0.95 1.78
    16  Wildcat_250mL            1.53      0.20     ab 0.42 1.11 1.95
    17  MatavenL_3.0L            1.59      0.21     ab 0.43 1.16 2.01
    18    Hussar_200g            1.66      0.20     ab 0.42 1.24 2.07
    19   Achieve_250g            1.68      0.21     ab 0.43 1.25 2.11
    20   Tristar_1.5L            1.90      0.21     ab 0.43 1.48 2.33
    21    Hussar_150g            2.01      0.21      b 0.42 1.59 2.43

# example 7 works

                   Df denDF     F.inc           Pr
    (Intercept)     1   6.9 188.10000 2.807176e-06
    Control         1  30.1   3.40000 7.504586e-02
    Herbicide       7  27.5   3.41500 9.356671e-03
    Rate            2  29.6   0.03766 9.630875e-01
    Herbicide:Rate 10  29.4   3.18000 7.021822e-03

---

                       component std.error z.ratio bound %ch
    Block                2.9e-07        NA      NA     B  NA
    Column:Row!R         1.8e-01     0.048     3.7     P 0.0
    Column:Row!Row!cor   5.4e-01     0.137     4.0     U 0.1

---

       Herbicide    Rate predicted.value std.error groups   ci  low   up
    1   Hoegrass     Low            0.92      0.20      a 0.42 0.50 1.34
    2    Wildcat  Medium            0.94      0.20      a 0.42 0.52 1.35
    3   Atlantis     Low            0.94      0.21     ab 0.43 0.51 1.37
    4    Wildcat    High            0.94      0.20      a 0.42 0.53 1.36
    5      Topik     Low            0.95      0.20      a 0.41 0.53 1.36
    6    Achieve  Medium            0.96      0.21     ab 0.43 0.54 1.39
    7    Control Control            0.98      0.21      a 0.42 0.56 1.41
    8   Hoegrass  Medium            1.06      0.20     ab 0.42 0.65 1.48
    9    Tristar     Low            1.07      0.20     ab 0.42 0.65 1.48
    10   Achieve    High            1.08      0.20     ab 0.42 0.67 1.50
    11  Atlantis    High            1.18      0.20     ab 0.42 0.77 1.60
    12     Topik    High            1.22      0.21     ab 0.42 0.80 1.64
    13  MatavenL     Low            1.23      0.21     ab 0.42 0.81 1.65
    14  Hoegrass    High            1.30      0.21     ab 0.42 0.88 1.72
    15     Topik  Medium            1.37      0.20     ab 0.42 0.95 1.78
    16   Wildcat     Low            1.53      0.20     ab 0.42 1.12 1.95
    17  MatavenL    High            1.59      0.21     ab 0.43 1.16 2.01
    18    Hussar    High            1.66      0.20     ab 0.41 1.24 2.07
    19   Achieve     Low            1.68      0.21     ab 0.43 1.25 2.11
    20   Tristar    High            1.90      0.21     ab 0.42 1.48 2.33
    21    Hussar     Low            2.01      0.21      b 0.42 1.59 2.43
    
    Aliased levels are: Achieve:Control, Atlantis:Control, Atlantis:Medium, Control:High, Control:Low, Control:Medium, Hoegrass:Control, Hussar:Control, Hussar:Medium, MatavenL:Control, MatavenL:Medium, Topik:Control, Tristar:Control, Tristar:Medium and Wildcat:Control 

# exercise 1 works

    Analysis of Variance Table
    
    Response: Yield
              Df  Sum Sq  Mean Sq F value    Pr(>F)    
    Variety   11 2.14254 0.194777  4.6796 0.0007707 ***
    Residuals 24 0.99893 0.041622                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

        Variety predicted.value std.error df groups     ci     low      up
    1      Lang         1.97333   0.11779 24      a 0.2431 1.73023 2.21644
    2  Drysdale         2.13000   0.11779 24      a 0.2431 1.88690 2.37310
    3     Wylah         2.13000   0.11779 24      a 0.2431 1.88690 2.37310
    4    Baxter         2.14000   0.11779 24      a 0.2431 1.89690 2.38310
    5      Janz         2.19333   0.11779 24     ab 0.2431 1.95023 2.43644
    6    Endure         2.24000   0.11779 24     ab 0.2431 1.99690 2.48310
    7     Orion         2.27000   0.11779 24     ab 0.2431 2.02690 2.51310
    8     Zippy         2.28333   0.11779 24     ab 0.2431 2.04023 2.52644
    9   Fortune         2.52667   0.11779 24     ab 0.2431 2.28356 2.76977
    10  Caryina         2.54000   0.11779 24     ab 0.2431 2.29690 2.78310
    11  Pugsley         2.75000   0.11779 24      b 0.2431 2.50690 2.99310
    12   Arrino         2.75333   0.11779 24      b 0.2431 2.51023 2.99644

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
      1        KC             2.1       0.2 18      a 0.4 1.7 2.5
      2        PE             2.2       0.2 18      a 0.4 1.8 2.5
      3        HL             2.6       0.2 18     ab 0.4 2.2 3.0
      4        CN             2.8       0.2 18     ab 0.4 2.4 3.1
      5        HE             2.8       0.2 18     ab 0.4 2.4 3.2
      6        CP             3.4       0.2 18      b 0.4 3.0 3.8

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
    1 CarolinaCross            2.84      0.42 24      a 0.87 1.97 3.71
    2       Pharoah            2.86      0.42 24      a 0.87 1.99 3.73
    3       Phantom            3.08      0.42 24     ab 0.87 2.21 3.95
    4      Hercules            4.70      0.42 24     ab 0.87 3.83 5.57
    5  Melitopolski            4.78      0.42 24      b 0.87 3.91 5.65
    6     Orangeglo            4.96      0.42 24      b 0.87 4.09 5.83
    7      Sudanese            8.88      0.42 24      c 0.87 8.01 9.75

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
    1      Root           31.61       1.4 18      a 2.94 28.67 34.54
    2      Seed           35.98       1.4 18     ab 2.94 33.04 38.91
    3     Stalk           38.95       1.4 18     bc 2.94 36.02 41.89
    4    Damage           43.52       1.4 18     cd 2.94 40.58 46.45
    5      Silk           48.12       1.4 18      d 2.94 45.18 51.05

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
    1        T4           16.01       1.6 10      a 3.56 12.45 19.57
    2        T8           17.51       1.6 10     ab 3.56 13.95 21.07
    3       T12           21.40       1.6 10     ab 3.56 17.85 24.96
    4        T0           24.39       1.6 10      b 3.56 20.83 27.95

# exercise 7 works

                Df denDF F.inc           Pr
    (Intercept)  1     4 65.47 1.267962e-03
    Variety      6    24 25.34 2.868344e-09

---

            Variety predicted.value std.error groups   ci  low    up
    1 CarolinaCross            2.84      0.69      a 1.42 1.42  4.26
    2       Pharoah            2.86      0.69      a 1.42 1.44  4.28
    3       Phantom            3.08      0.69     ab 1.42 1.66  4.50
    4      Hercules            4.70      0.69     ab 1.42 3.28  6.12
    5  Melitopolski            4.78      0.69      b 1.42 3.36  6.20
    6     Orangeglo            4.96      0.69      b 1.42 3.54  6.38
    7      Sudanese            8.88      0.69      c 1.42 7.46 10.30

# exercise 8 works

                Df denDF   F.inc           Pr
    (Intercept)  1     3 910.800 7.990756e-05
    SeedingRate  5    15   1.321 3.075796e-01

# exercise 9 works

                Df denDF  F.inc           Pr
    (Intercept)  1    20 4333.0 0.000000e+00
    Treatment    4    20   22.8 3.258972e-07

---

      Treatment predicted.value std.error groups   ci   low    up
    1      Root           31.61      1.35      a 2.81 28.80 34.42
    2      Seed           35.98      1.35     ab 2.81 33.17 38.79
    3     Stalk           38.95      1.35     bc 2.81 36.14 41.76
    4    Damage           43.52      1.35     cd 2.81 40.71 46.33
    5      Silk           48.12      1.35      d 2.81 45.31 50.93

# exercise 10 works

                Df denDF   F.inc         Pr
    (Intercept)  1   0.7 124.100 0.11595886
    Treatment    3  10.0   5.659 0.01573632

---

      Treatment predicted.value std.error groups   ci   low    up
    1        T4           16.01       1.6      a 3.56 12.45 19.57
    2        T8           17.51       1.6     ab 3.56 13.95 21.07
    3       T12           21.40       1.6     ab 3.56 17.85 24.96
    4        T0           24.39       1.6      b 3.56 20.83 27.95

# exercise 11 works

                      Df denDF    F.inc           Pr
    (Intercept)        1   5.0 245.1000 1.931825e-05
    Genotype           2  54.3   3.2210 4.765853e-02
    Nitrogen           3  43.2  27.0500 5.270026e-10
    Genotype:Nitrogen  6  50.1   0.2233 9.674091e-01

---

        Genotype predicted.value std.error groups    ci   low     up
    1    Victory           97.68      7.15      a 14.34 83.34 112.02
    2 GoldenRain          104.89      7.15     ab 14.34 90.55 119.23
    3 Marvellous          109.35      7.16      b 14.35 95.00 123.70

---

      Nitrogen predicted.value std.error groups    ci    low     up
    1        0           79.39      7.37      a 14.87  64.52  94.26
    2      0.2           98.89      7.37      b 14.87  84.02 113.76
    3      0.4          114.22      7.37      c 14.87  99.35 129.09
    4      0.6          123.39      7.37      c 14.87 108.52 138.26

# exercise 12 works

                       Df denDF  F.inc          Pr
    (Intercept)         1     2 48.000 0.020205048
    Variety             4    16  6.943 0.001937025
    Irrigation          1     2 22.820 0.041135282
    Variety:Irrigation  4    16  3.994 0.019643763

---

       Variety Irrigation predicted.value std.error groups  ci  low   up
    1  Thumper    Rainfed            4.61      0.99      a 2.1 2.51 6.71
    2  Cobbler    Rainfed            5.47      0.99     ab 2.1 3.37 7.57
    3    Bravo    Rainfed            5.91      0.99      b 2.1 3.81 8.01
    4    Hyola    Rainfed            6.19      0.99     bc 2.1 4.09 8.29
    5  Victory    Rainfed            6.43      0.99     bc 2.1 4.33 8.53
    6  Thumper  Irrigated            6.92      0.99     bc 2.1 4.82 9.02
    7  Victory  Irrigated            7.02      0.99     bc 2.1 4.92 9.12
    8    Bravo  Irrigated            7.68      0.99      c 2.1 5.58 9.78
    9    Hyola  Irrigated            7.70      0.99      c 2.1 5.60 9.80
    10 Cobbler  Irrigated            7.75      0.99      c 2.1 5.65 9.85

# exercise 13 works

                      Df denDF  F.inc       Pr
    (Intercept)        1   5.0 254.40 1.83e-05
    Genotype           2  53.6   4.63 1.39e-02
    Nitrogen           3  21.3  33.70 2.86e-08
    Genotype:Nitrogen  6  50.3   0.30 9.34e-01

---

        Genotype predicted.value std.error groups    ci   low     up
    1    Victory           97.41      7.04      a 14.12 83.29 111.53
    2 GoldenRain          104.31      7.05     ab 14.15 90.16 118.46
    3 Marvellous          110.07      7.04      b 14.13 95.94 124.19

---

      Nitrogen predicted.value std.error groups    ci    low     up
    1        0           79.44      7.11      a 14.78  64.67  94.22
    2      0.2           98.82      7.11      b 14.78  84.04 113.60
    3      0.4          114.08      7.11      c 14.78  99.30 128.87
    4      0.6          123.37      7.10      c 14.76 108.62 138.13

# exercise 14 works

                Df denDF     F.inc           Pr
    (Intercept)  1  17.7 23710.000 0.000000e+00
    Genotype    49  69.4     5.089 4.795863e-10

---

       Genotype predicted.value std.error   groups   ci  low   up
    1       G36            3.68      0.09        a 0.18 3.50 3.86
    2       G28            3.79      0.09       ab 0.18 3.61 3.97
    3       G30            3.90      0.09      abc 0.18 3.72 4.08
    4       G46            3.91      0.09     abcd 0.18 3.73 4.08
    5       G23            3.92      0.09    abcde 0.18 3.74 4.10
    6       G29            3.96      0.09   abcdef 0.18 3.78 4.13
    7       G07            3.96      0.09   abcdef 0.18 3.78 4.13
    8       G21            3.98      0.09  abcdefg 0.18 3.80 4.16
    9       G45            3.98      0.09  abcdefg 0.18 3.80 4.16
    10      G33            4.02      0.09 abcdefgh 0.18 3.84 4.20
    11      G49            4.03      0.09 abcdefgh 0.18 3.86 4.21
    12      G06            4.04      0.09 abcdefgh 0.18 3.86 4.21
    13      G38            4.06      0.09 abcdefgh 0.18 3.88 4.24
    14      G27            4.06      0.09 abcdefgh 0.18 3.88 4.24
    15      G43            4.06      0.09 abcdefgh 0.18 3.89 4.24
    16      G19            4.07      0.09 abcdefgh 0.18 3.89 4.25
    17      G15            4.12      0.09 abcdefgh 0.18 3.95 4.30
    18      G31            4.13      0.09 abcdefgh 0.18 3.95 4.31
    19      G24            4.13      0.09 abcdefgh 0.18 3.95 4.31
    20      G48            4.15      0.09 abcdefgh 0.18 3.97 4.33
    21      G34            4.15      0.09 abcdefgh 0.18 3.97 4.33
    22      G05            4.15      0.09 abcdefgh 0.18 3.97 4.33
    23      G18            4.16      0.09 abcdefgh 0.18 3.98 4.34
    24      G37            4.16      0.09  bcdefgh 0.18 3.98 4.33
    25      G47            4.18      0.09 abcdefgh 0.18 4.00 4.36
    26      G08            4.18      0.09 abcdefgh 0.18 4.00 4.36
    27      G25            4.19      0.09  bcdefgh 0.18 4.02 4.37
    28      G26            4.19      0.09 abcdefgh 0.18 4.02 4.37
    29      G13            4.20      0.09  bcdefgh 0.18 4.02 4.37
    30      G35            4.21      0.09  bcdefgh 0.18 4.03 4.38
    31      G44            4.22      0.09  bcdefgh 0.18 4.04 4.40
    32      G39            4.23      0.09  bcdefgh 0.18 4.05 4.41
    33      G14            4.26      0.09  bcdefgh 0.18 4.08 4.44
    34      G41            4.27      0.09  bcdefgh 0.18 4.09 4.45
    35      G01            4.31      0.09  bcdefgh 0.19 4.12 4.49
    36      G10            4.31      0.09   cdefgh 0.18 4.13 4.49
    37      G03            4.33      0.09   cdefgh 0.18 4.15 4.50
    38      G16            4.33      0.09   cdefgh 0.18 4.15 4.51
    39      G32            4.33      0.09   cdefgh 0.18 4.16 4.51
    40      G22            4.34      0.09   cdefgh 0.18 4.17 4.52
    41      G20            4.36      0.09   cdefgh 0.18 4.18 4.54
    42      G02            4.36      0.09   cdefgh 0.18 4.19 4.54
    43      G40            4.38      0.09   cdefgh 0.18 4.19 4.56
    44      G11            4.38      0.09   cdefgh 0.18 4.20 4.57
    45      G42            4.41      0.09    defgh 0.18 4.24 4.59
    46      G09            4.42      0.09     efgh 0.18 4.24 4.60
    47      G50            4.43      0.09     efgh 0.18 4.25 4.61
    48      G17            4.43      0.09      fgh 0.18 4.26 4.61
    49      G12            4.49      0.09       gh 0.18 4.30 4.67
    50      G04            4.52      0.09        h 0.18 4.34 4.70

# exercise 15 works

                Df denDF   F.inc           Pr
    (Intercept)  1   4.7 433.300 8.380984e-06
    Control      1  15.2  17.600 7.572642e-04
    Season       1  16.1   7.289 1.570468e-02
    Rate         2  15.0   6.489 9.355453e-03
    Season:Rate  2  20.8   1.352 2.804897e-01

---

      Control predicted.value std.error groups   ci  low   up
    1      No            2.37      0.13      a 0.27 2.10 2.64
    2     Yes            3.06      0.17      b 0.37 2.69 3.42

---

      Rate predicted.value std.error groups   ci  low   up
    1   12            1.99      0.17      a 0.36 1.64 2.35
    2    3            2.48      0.17     ab 0.36 2.12 2.83
    3    6            2.62      0.17     bc 0.36 2.26 2.98
    4    0            3.06      0.17      c 0.37 2.69 3.43

---

      Season predicted.value std.error groups   ci  low   up
    1      F            2.14      0.15      a 0.31 1.83 2.45
    2      S            2.59      0.15      b 0.32 2.27 2.91
    3      O            3.06      0.17      c 0.36 2.70 3.42

