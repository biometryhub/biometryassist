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
    1  20            9.96       0.4 16      a 1.16  8.81 11.12
    2   1           12.26       0.4 16      b 1.16 11.10 13.42
    3   5           16.14       0.4 16      c 1.16 14.99 17.30
    4  10           17.77       0.4 16      c 1.16 16.62 18.93

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
    1  T3         11.1525    0.3319 77      a 1.0047 10.1478 12.1572
    2  T5         12.4542    0.3319 77      a 1.0047 11.4494 13.4589
    3  T6         14.0250    0.3319 77      b 1.0047 13.0203 15.0297
    4  T4         15.1008    0.3319 77     bc 1.0047 14.0961 16.1056
    5  T2         16.1117    0.3319 77     cd 1.0047 15.1069 17.1164
    6  T7         17.2400    0.3319 77     de 1.0047 16.2353 18.2447
    7  T1         17.8292    0.3319 77      e 1.0047 16.8244 18.8339

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

        Variety predicted.value std.error df groups   ci   low   up
    1 Parafield            1.68      0.63 12      a 1.86 -0.18 3.54
    2     Kaspa            2.68      0.63 12     ab 1.86  0.81 4.54
    3    Yarrum            4.72      0.63 12      b 1.86  2.86 6.59
    4    Excell            4.85      0.63 12      b 1.86  2.99 6.71

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
    1  S4         1707.94     61.98  6      a 214.54 1493.40 1922.48
    2  S2         1802.70     61.98  6     ab 214.54 1588.16 2017.24
    3  S1         2053.73     61.98  6     bc 214.54 1839.19 2268.27
    4  S3         2200.08     61.98  6      c 214.54 1985.54 2414.63

# example 3 LMM works

                Df denDF  F.inc          Pr
    (Intercept)  1     4 33.230 0.004494493
    Variety      3    12  6.187 0.008747377

---

        Variety predicted.value std.error groups   ci   low   up
    1 Parafield            1.68      0.81      a 2.41 -0.73 4.09
    2     Kaspa            2.68      0.81     ab 2.41  0.26 5.09
    3    Yarrum            4.72      0.81      b 2.41  2.31 7.14
    4    Excell            4.85      0.81      b 2.41  2.44 7.26

# example 4 LMM works

                Df denDF   F.inc           Pr
    (Intercept)  1     3 3053.00 1.305451e-05
    trt          3     9   13.33 1.162778e-03

---

      trt predicted.value std.error groups     ci     low      up
    1  S4         1707.94     64.09      a 200.08 1507.86 1908.02
    2  S2         1802.70     64.09     ab 200.08 1602.61 2002.78
    3  S1         2053.73     64.09     bc 200.08 1853.65 2253.82
    4  S3         2200.09     64.09      c 200.08 2000.00 2400.17

# example 5 works

                       Df denDF     F.inc           Pr
    (Intercept)         1     3 3029.0000 1.321435e-05
    Genotype           69   414    7.2010 0.000000e+00
    Fungicide           1     3   40.2700 7.914935e-03
    Genotype:Fungicide 69   414    0.9331 6.290102e-01

---

       Genotype predicted.value std.error    groups   ci  low   up
    1       G04            4.14      0.14         a 0.57 3.57 4.72
    2       G14            4.63      0.14        ab 0.57 4.06 5.21
    3       G20            4.80      0.14        bc 0.57 4.22 5.37
    4       G10            4.81      0.14       bcd 0.57 4.24 5.39
    5       G28            4.84      0.14      bcde 0.57 4.27 5.41
    6       G24            4.85      0.14     bcdef 0.57 4.28 5.43
    7       G01            4.91      0.14    bcdefg 0.57 4.34 5.48
    8       G05            4.94      0.14   bcdefgh 0.57 4.37 5.52
    9       G49            4.95      0.14   bcdefgh 0.57 4.37 5.52
    10      G34            5.01      0.14   bcdefgh 0.57 4.43 5.58
    11      G25            5.02      0.14   bcdefgh 0.57 4.44 5.59
    12      G30            5.04      0.14  bcdefghi 0.57 4.47 5.62
    13      G21            5.05      0.14  bcdefghi 0.57 4.47 5.62
    14      G02            5.06      0.14  bcdefghi 0.57 4.49 5.64
    15      G15            5.07      0.14  bcdefghi 0.57 4.50 5.64
    16      G41            5.09      0.14  bcdefghi 0.57 4.51 5.66
    17      G08            5.09      0.14  bcdefghi 0.57 4.52 5.66
    18      G07            5.10      0.14  bcdefghi 0.57 4.53 5.67
    19      G45            5.11      0.14  bcdefghi 0.57 4.53 5.68
    20      G06            5.13      0.14  bcdefghi 0.57 4.55 5.70
    21      G55            5.14      0.14  bcdefghi 0.57 4.57 5.72
    22      G32            5.16      0.14 bcdefghij 0.57 4.58 5.73
    23      G11            5.16      0.14 bcdefghij 0.57 4.59 5.74
    24      G39            5.18      0.14 bcdefghij 0.57 4.61 5.75
    25      G61            5.20      0.14 bcdefghij 0.57 4.63 5.77
    26      G53            5.21      0.14 bcdefghij 0.57 4.64 5.78
    27      G37            5.21      0.14 bcdefghij 0.57 4.64 5.78
    28      G26            5.21      0.14 bcdefghij 0.57 4.64 5.79
    29      G43            5.21      0.14 bcdefghij 0.57 4.64 5.79
    30      G35            5.22      0.14 bcdefghij 0.57 4.64 5.79
    31      G66            5.22      0.14 bcdefghij 0.57 4.64 5.79
    32      G16            5.23      0.14  cdefghij 0.57 4.65 5.80
    33      G68            5.25      0.14  cdefghij 0.57 4.68 5.82
    34      G46            5.25      0.14  cdefghij 0.57 4.68 5.82
    35      G29            5.26      0.14  cdefghij 0.57 4.69 5.84
    36      G47            5.27      0.14  cdefghij 0.57 4.70 5.84
    37      G44            5.29      0.14  cdefghij 0.57 4.72 5.86
    38      G23            5.29      0.14  cdefghij 0.57 4.72 5.87
    39      G56            5.29      0.14  cdefghij 0.57 4.72 5.87
    40      G58            5.31      0.14  cdefghij 0.57 4.73 5.88
    41      G57            5.31      0.14  cdefghij 0.57 4.74 5.89
    42      G63            5.32      0.14  cdefghij 0.57 4.74 5.89
    43      G62            5.32      0.14  cdefghij 0.57 4.75 5.89
    44      G54            5.33      0.14  cdefghij 0.57 4.76 5.91
    45      G51            5.36      0.14  cdefghij 0.57 4.78 5.93
    46      G67            5.37      0.14  cdefghij 0.57 4.80 5.94
    47      G22            5.37      0.14  cdefghij 0.57 4.80 5.94
    48      G42            5.37      0.14  cdefghij 0.57 4.80 5.94
    49      G59            5.37      0.14  cdefghij 0.57 4.80 5.94
    50      G65            5.38      0.14  cdefghij 0.57 4.80 5.95
    51      G69            5.38      0.14  cdefghij 0.57 4.81 5.96
    52      G38            5.38      0.14  cdefghij 0.57 4.81 5.96
    53      G12            5.39      0.14   defghij 0.57 4.82 5.97
    54      G31            5.41      0.14    efghij 0.57 4.84 5.98
    55      G50            5.42      0.14    efghij 0.57 4.85 6.00
    56      G17            5.42      0.14    efghij 0.57 4.85 6.00
    57      G52            5.43      0.14    efghij 0.57 4.85 6.00
    58      G64            5.44      0.14     fghij 0.57 4.87 6.01
    59      G13            5.45      0.14      ghij 0.57 4.87 6.02
    60      G70            5.45      0.14      ghij 0.57 4.88 6.03
    61      G09            5.46      0.14      ghij 0.57 4.88 6.03
    62      G60            5.48      0.14      ghij 0.57 4.91 6.05
    63      G27            5.49      0.14     ghijk 0.57 4.92 6.06
    64      G18            5.50      0.14     ghijk 0.57 4.92 6.07
    65      G36            5.50      0.14      hijk 0.57 4.93 6.08
    66      G48            5.51      0.14      hijk 0.57 4.94 6.08
    67      G40            5.53      0.14      hijk 0.57 4.95 6.10
    68      G33            5.61      0.14       ijk 0.57 5.04 6.18
    69      G19            5.73      0.14        jk 0.57 5.16 6.31
    70      G03            6.07      0.14         k 0.57 5.50 6.65

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
    1  Hoegrass_0.75L            0.92      0.20      a 0.80 0.12 1.73
    2   Wildcat_300mL            0.94      0.20      a 0.80 0.14 1.74
    3  Atlantis_300mL            0.94      0.21     ab 0.82 0.12 1.76
    4   Wildcat_350mL            0.94      0.20      a 0.80 0.14 1.75
    5      Topik_50mL            0.95      0.20      a 0.80 0.15 1.74
    6    Achieve_300g            0.96      0.21     ab 0.82 0.14 1.78
    7       Control_0            0.98      0.21      a 0.82 0.17 1.80
    8   Hoegrass_1.0L            1.06      0.20     ab 0.80 0.26 1.86
    9    Tristar_1.0L            1.07      0.20     ab 0.80 0.27 1.87
    10   Achieve_380g            1.08      0.20     ab 0.80 0.28 1.89
    11 Atlantis_330mL            1.18      0.20     ab 0.80 0.39 1.98
    12     Topik_85mL            1.22      0.21     ab 0.81 0.41 2.03
    13 MatavenL_2.25L            1.23      0.21     ab 0.81 0.42 2.04
    14  Hoegrass_1.2L            1.30      0.21     ab 0.81 0.49 2.11
    15     Topik_65mL            1.37      0.20     ab 0.80 0.56 2.17
    16  Wildcat_250mL            1.53      0.20     ab 0.80 0.73 2.34
    17  MatavenL_3.0L            1.59      0.21     ab 0.82 0.77 2.41
    18    Hussar_200g            1.66      0.20     ab 0.80 0.86 2.45
    19   Achieve_250g            1.68      0.21     ab 0.82 0.86 2.50
    20   Tristar_1.5L            1.90      0.21     ab 0.82 1.08 2.72
    21    Hussar_150g            2.01      0.21      b 0.81 1.20 2.81

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
    1   Hoegrass     Low            0.92      0.20      a 0.80 0.12 1.72
    2    Wildcat  Medium            0.94      0.20      a 0.80 0.14 1.73
    3   Atlantis     Low            0.94      0.21     ab 0.82 0.12 1.76
    4    Wildcat    High            0.94      0.20      a 0.80 0.15 1.74
    5      Topik     Low            0.95      0.20      a 0.79 0.15 1.74
    6    Achieve  Medium            0.96      0.21     ab 0.81 0.15 1.77
    7    Control Control            0.98      0.21      a 0.81 0.17 1.79
    8   Hoegrass  Medium            1.06      0.20     ab 0.79 0.27 1.86
    9    Tristar     Low            1.07      0.20     ab 0.80 0.27 1.86
    10   Achieve    High            1.08      0.20     ab 0.80 0.29 1.88
    11  Atlantis    High            1.18      0.20     ab 0.79 0.39 1.98
    12     Topik    High            1.22      0.21     ab 0.80 0.41 2.02
    13  MatavenL     Low            1.23      0.21     ab 0.80 0.42 2.03
    14  Hoegrass    High            1.30      0.21     ab 0.81 0.49 2.10
    15     Topik  Medium            1.37      0.20     ab 0.80 0.57 2.17
    16   Wildcat     Low            1.53      0.20     ab 0.80 0.73 2.33
    17  MatavenL    High            1.59      0.21     ab 0.81 0.77 2.40
    18    Hussar    High            1.66      0.20     ab 0.79 0.87 2.45
    19   Achieve     Low            1.68      0.21     ab 0.82 0.86 2.49
    20   Tristar    High            1.90      0.21     ab 0.81 1.09 2.71
    21    Hussar     Low            2.01      0.21      b 0.80 1.21 2.81
    
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
    1      Lang         1.97333   0.11779 24      a 0.4247 1.54863 2.39803
    2  Drysdale         2.13000   0.11779 24      a 0.4247 1.70530 2.55470
    3     Wylah         2.13000   0.11779 24      a 0.4247 1.70530 2.55470
    4    Baxter         2.14000   0.11779 24      a 0.4247 1.71530 2.56470
    5      Janz         2.19333   0.11779 24     ab 0.4247 1.76863 2.61803
    6    Endure         2.24000   0.11779 24     ab 0.4247 1.81530 2.66470
    7     Orion         2.27000   0.11779 24     ab 0.4247 1.84530 2.69470
    8     Zippy         2.28333   0.11779 24     ab 0.4247 1.85863 2.70803
    9   Fortune         2.52667   0.11779 24     ab 0.4247 2.10197 2.95137
    10  Caryina         2.54000   0.11779 24     ab 0.4247 2.11530 2.96470
    11  Pugsley         2.75000   0.11779 24      b 0.4247 2.32530 3.17470
    12   Arrino         2.75333   0.11779 24      b 0.4247 2.32863 3.17803

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
      1        KC             2.1       0.2 18      a 0.6 1.6 2.7
      2        PE             2.2       0.2 18      a 0.6 1.6 2.7
      3        HL             2.6       0.2 18     ab 0.6 2.0 3.2
      4        CN             2.8       0.2 18     ab 0.6 2.2 3.4
      5        HE             2.8       0.2 18     ab 0.6 2.2 3.4
      6        CP             3.4       0.2 18      b 0.6 2.8 3.9

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

            Variety predicted.value std.error df groups   ci  low    up
    1 CarolinaCross            2.84      0.42 24      a 1.35 1.49  4.19
    2       Pharoah            2.86      0.42 24      a 1.35 1.51  4.21
    3       Phantom            3.08      0.42 24     ab 1.35 1.73  4.43
    4      Hercules            4.70      0.42 24     ab 1.35 3.35  6.05
    5  Melitopolski            4.78      0.42 24      b 1.35 3.43  6.13
    6     Orangeglo            4.96      0.42 24      b 1.35 3.61  6.31
    7      Sudanese            8.88      0.42 24      c 1.35 7.53 10.23

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
    1      Root           31.61       1.4 18      a 4.23 27.38 35.84
    2      Seed           35.98       1.4 18     ab 4.23 31.75 40.21
    3     Stalk           38.95       1.4 18     bc 4.23 34.72 43.18
    4    Damage           43.52       1.4 18     cd 4.23 39.29 47.75
    5      Silk           48.12       1.4 18      d 4.23 43.89 52.35

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
    1        T4           16.01       1.6 10      a 4.88 11.13 20.89
    2        T8           17.51       1.6 10     ab 4.88 12.63 22.39
    3       T12           21.40       1.6 10     ab 4.88 16.52 26.28
    4        T0           24.39       1.6 10      b 4.88 19.51 29.27

# exercise 7 works

                Df denDF F.inc           Pr
    (Intercept)  1     4 65.47 1.267962e-03
    Variety      6    24 25.34 2.868344e-09

---

            Variety predicted.value std.error groups   ci  low    up
    1 CarolinaCross            2.84      0.69      a 2.21 0.63  5.05
    2       Pharoah            2.86      0.69      a 2.21 0.65  5.07
    3       Phantom            3.08      0.69     ab 2.21 0.87  5.29
    4      Hercules            4.70      0.69     ab 2.21 2.49  6.91
    5  Melitopolski            4.78      0.69      b 2.21 2.57  6.99
    6     Orangeglo            4.96      0.69      b 2.21 2.75  7.17
    7      Sudanese            8.88      0.69      c 2.21 6.67 11.09

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
    1      Root           31.61      1.35      a 4.03 27.58 35.64
    2      Seed           35.98      1.35     ab 4.03 31.95 40.01
    3     Stalk           38.95      1.35     bc 4.03 34.92 42.98
    4    Damage           43.52      1.35     cd 4.03 39.49 47.55
    5      Silk           48.12      1.35      d 4.03 44.09 52.15

# exercise 10 works

                Df denDF   F.inc         Pr
    (Intercept)  1   0.7 124.100 0.11595886
    Treatment    3  10.0   5.659 0.01573632

---

      Treatment predicted.value std.error groups   ci   low    up
    1        T4           16.01       1.6      a 4.88 11.13 20.89
    2        T8           17.51       1.6     ab 4.88 12.63 22.39
    3       T12           21.40       1.6     ab 4.88 16.52 26.28
    4        T0           24.39       1.6      b 4.88 19.51 29.27

# exercise 11 works

                      Df denDF    F.inc           Pr
    (Intercept)        1   5.0 245.1000 1.931825e-05
    Genotype           2  54.3   3.2210 4.765853e-02
    Nitrogen           3  43.2  27.0500 5.270026e-10
    Genotype:Nitrogen  6  50.1   0.2233 9.674091e-01

---

        Genotype predicted.value std.error groups    ci   low     up
    1    Victory           97.68      7.15      a 17.24 80.44 114.92
    2 GoldenRain          104.89      7.15     ab 17.23 87.66 122.12
    3 Marvellous          109.35      7.16      b 17.25 92.10 126.60

---

      Nitrogen predicted.value std.error groups   ci    low     up
    1        0           79.39      7.37      a 19.7  59.69  99.09
    2      0.2           98.89      7.37      b 19.7  79.19 118.59
    3      0.4          114.22      7.37      c 19.7  94.52 133.93
    4      0.6          123.39      7.37      c 19.7 103.69 143.09

# exercise 12 works

                       Df denDF  F.inc          Pr
    (Intercept)         1     2 48.000 0.020205048
    Variety             4    16  6.943 0.001937025
    Irrigation          1     2 22.820 0.041135282
    Variety:Irrigation  4    16  3.994 0.019643763

---

       Variety Irrigation predicted.value std.error groups   ci  low    up
    1  Thumper    Rainfed            4.61      0.99      a 3.61 1.01  8.22
    2  Cobbler    Rainfed            5.47      0.99     ab 3.61 1.86  9.08
    3    Bravo    Rainfed            5.91      0.99      b 3.61 2.30  9.51
    4    Hyola    Rainfed            6.19      0.99     bc 3.61 2.59  9.80
    5  Victory    Rainfed            6.43      0.99     bc 3.61 2.83 10.04
    6  Thumper  Irrigated            6.92      0.99     bc 3.61 3.32 10.53
    7  Victory  Irrigated            7.02      0.99     bc 3.61 3.42 10.63
    8    Bravo  Irrigated            7.68      0.99      c 3.61 4.08 11.29
    9    Hyola  Irrigated            7.70      0.99      c 3.61 4.10 11.31
    10 Cobbler  Irrigated            7.75      0.99      c 3.61 4.15 11.36

# exercise 13 works

                      Df denDF  F.inc       Pr
    (Intercept)        1   5.0 254.40 1.83e-05
    Genotype           2  53.6   4.63 1.39e-02
    Nitrogen           3  21.3  33.70 2.86e-08
    Genotype:Nitrogen  6  50.3   0.30 9.34e-01

---

        Genotype predicted.value std.error groups    ci   low     up
    1    Victory           97.41      7.04      a 16.98 80.43 114.39
    2 GoldenRain          104.31      7.05     ab 17.01 87.30 121.32
    3 Marvellous          110.07      7.04      b 16.98 93.09 127.05

---

      Nitrogen predicted.value std.error groups    ci    low     up
    1        0           79.44      7.11      a 19.80  59.64  99.24
    2      0.2           98.82      7.11      b 19.80  79.01 118.62
    3      0.4          114.08      7.11      c 19.81  94.28 133.89
    4      0.6          123.37      7.10      c 19.77 103.60 143.14

# exercise 14 works

                Df denDF     F.inc           Pr
    (Intercept)  1  17.7 23710.000 0.000000e+00
    Genotype    49  69.4     5.089 4.795863e-10

---

       Genotype predicted.value std.error   groups   ci  low   up
    1       G36            3.68      0.09        a 0.38 3.30 4.06
    2       G28            3.79      0.09       ab 0.38 3.41 4.16
    3       G30            3.90      0.09      abc 0.37 3.53 4.27
    4       G46            3.91      0.09     abcd 0.37 3.54 4.28
    5       G23            3.92      0.09    abcde 0.38 3.54 4.30
    6       G29            3.96      0.09   abcdef 0.37 3.59 4.33
    7       G07            3.96      0.09   abcdef 0.37 3.59 4.33
    8       G21            3.98      0.09  abcdefg 0.37 3.61 4.35
    9       G45            3.98      0.09  abcdefg 0.37 3.61 4.35
    10      G33            4.02      0.09 abcdefgh 0.38 3.64 4.39
    11      G49            4.03      0.09 abcdefgh 0.37 3.66 4.40
    12      G06            4.04      0.09 abcdefgh 0.37 3.66 4.41
    13      G38            4.06      0.09 abcdefgh 0.37 3.69 4.43
    14      G27            4.06      0.09 abcdefgh 0.38 3.68 4.44
    15      G43            4.06      0.09 abcdefgh 0.37 3.69 4.43
    16      G19            4.07      0.09 abcdefgh 0.37 3.70 4.44
    17      G15            4.12      0.09 abcdefgh 0.37 3.75 4.50
    18      G31            4.13      0.09 abcdefgh 0.37 3.76 4.50
    19      G24            4.13      0.09 abcdefgh 0.38 3.75 4.51
    20      G48            4.15      0.09 abcdefgh 0.37 3.78 4.52
    21      G34            4.15      0.09 abcdefgh 0.38 3.77 4.53
    22      G05            4.15      0.09 abcdefgh 0.37 3.78 4.52
    23      G18            4.16      0.09 abcdefgh 0.38 3.78 4.54
    24      G37            4.16      0.09  bcdefgh 0.37 3.79 4.53
    25      G47            4.18      0.09 abcdefgh 0.37 3.81 4.55
    26      G08            4.18      0.09 abcdefgh 0.38 3.80 4.56
    27      G25            4.19      0.09  bcdefgh 0.37 3.82 4.56
    28      G26            4.19      0.09 abcdefgh 0.37 3.82 4.57
    29      G13            4.20      0.09  bcdefgh 0.37 3.83 4.57
    30      G35            4.21      0.09  bcdefgh 0.37 3.84 4.58
    31      G44            4.22      0.09  bcdefgh 0.37 3.85 4.59
    32      G39            4.23      0.09  bcdefgh 0.39 3.84 4.62
    33      G14            4.26      0.09  bcdefgh 0.38 3.88 4.64
    34      G41            4.27      0.09  bcdefgh 0.38 3.89 4.65
    35      G01            4.31      0.09  bcdefgh 0.39 3.92 4.70
    36      G10            4.31      0.09   cdefgh 0.38 3.93 4.69
    37      G03            4.33      0.09   cdefgh 0.37 3.96 4.70
    38      G16            4.33      0.09   cdefgh 0.38 3.95 4.71
    39      G32            4.33      0.09   cdefgh 0.37 3.96 4.70
    40      G22            4.34      0.09   cdefgh 0.37 3.97 4.72
    41      G20            4.36      0.09   cdefgh 0.37 3.99 4.73
    42      G02            4.36      0.09   cdefgh 0.37 3.99 4.73
    43      G40            4.38      0.09   cdefgh 0.38 3.99 4.76
    44      G11            4.38      0.09   cdefgh 0.38 4.01 4.76
    45      G42            4.41      0.09    defgh 0.37 4.04 4.78
    46      G09            4.42      0.09     efgh 0.37 4.05 4.79
    47      G50            4.43      0.09     efgh 0.38 4.05 4.81
    48      G17            4.43      0.09      fgh 0.37 4.06 4.80
    49      G12            4.49      0.09       gh 0.38 4.11 4.86
    50      G04            4.52      0.09        h 0.37 4.15 4.89

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
    1   12            1.99      0.17      a 0.48 1.51 2.48
    2    3            2.48      0.17     ab 0.48 2.00 2.96
    3    6            2.62      0.17     bc 0.49 2.14 3.11
    4    0            3.06      0.17      c 0.50 2.56 3.55

---

      Season predicted.value std.error groups   ci  low   up
    1      F            2.14      0.15      a 0.38 1.76 2.52
    2      S            2.59      0.15      b 0.39 2.20 2.98
    3      O            3.06      0.17      c 0.44 2.62 3.50

