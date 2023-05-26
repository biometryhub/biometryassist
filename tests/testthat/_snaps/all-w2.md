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
    3  20            9.96       0.4 16      a 0.71  9.26 10.67
    1   1           12.26       0.4 16      b 0.71 11.55 12.96
    4   5           16.14       0.4 16      c 0.71 15.44 16.85
    2  10           17.77       0.4 16      c 0.71 17.07 18.48

# example 2 works

    Analysis of Variance Table
    
    Response: TuberLengthGrowth
              Df Sum Sq Mean Sq F value    Pr(>F)    
    trt        6 436.01  72.668  54.989 < 2.2e-16 ***
    Residuals 77 101.76   1.322                      
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

      trt predicted.value std.error df groups   ci   low    up
    3  T3           11.15      0.33 77      a 0.55 10.60 11.70
    5  T5           12.45      0.33 77      a 0.55 11.90 13.01
    6  T6           14.02      0.33 77      b 0.55 13.47 14.58
    4  T4           15.10      0.33 77     bc 0.55 14.55 15.65
    2  T2           16.11      0.33 77     cd 0.55 15.56 16.66
    7  T7           17.24      0.33 77     de 0.55 16.69 17.79
    1  T1           17.83      0.33 77      e 0.55 17.28 18.38

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
    3 Parafield            1.68      0.63 12      a 1.12 0.56 2.80
    2     Kaspa            2.68      0.63 12     ab 1.12 1.56 3.79
    4    Yarrum            4.72      0.63 12      b 1.12 3.61 5.84
    1    Excell            4.85      0.63 12      b 1.12 3.73 5.97

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
    4  S4         1707.94     61.98  6      a 120.43 1587.51 1828.37
    2  S2         1802.70     61.98  6     ab 120.43 1682.27 1923.13
    1  S1         2053.73     61.98  6     bc 120.43 1933.30 2174.16
    3  S3         2200.08     61.98  6      c 120.43 2079.66 2320.51

# example 3 LMM works

                Df denDF  F.inc          Pr
    (Intercept)  1     4 33.230 0.004494493
    Variety      3    12  6.187 0.008747380

---

        Variety predicted.value std.error groups   ci  low   up
    3 Parafield            1.68      0.81      a 1.45 0.23 3.13
    2     Kaspa            2.68      0.81     ab 1.45 1.23 4.12
    4    Yarrum            4.72      0.81      b 1.45 3.28 6.17
    1    Excell            4.85      0.81      b 1.45 3.40 6.30

# example 4 LMM works

                Df denDF   F.inc           Pr
    (Intercept)  1     3 3053.00 1.305451e-05
    trt          3     9   13.33 1.162778e-03

---

      trt predicted.value std.error groups     ci     low      up
    4  S4         1707.94     64.09      a 117.49 1590.45 1825.43
    2  S2         1802.70     64.09     ab 117.49 1685.21 1920.19
    1  S1         2053.73     64.09     bc 117.49 1936.24 2171.22
    3  S3         2200.08     64.09      c 117.49 2082.60 2317.57

# example 5 works

                       Df denDF     F.inc           Pr
    (Intercept)         1     3 3029.0000 1.321435e-05
    Genotype           69   414    7.2010 2.459456e-39
    Fungicide           1     3   40.2700 7.914935e-03
    Genotype:Fungicide 69   414    0.9331 6.290098e-01

---

       Genotype predicted.value std.error    groups   ci  low   up
    4       G04            4.15      0.14         a 0.23 3.92 4.37
    14      G14            4.63      0.14        ab 0.23 4.41 4.86
    20      G20            4.80      0.14        bc 0.23 4.57 5.02
    10      G10            4.81      0.14       bcd 0.23 4.59 5.04
    28      G28            4.84      0.14      bcde 0.23 4.61 5.07
    24      G24            4.86      0.14     bcdef 0.23 4.63 5.08
    1       G01            4.91      0.14    bcdefg 0.23 4.68 5.14
    5       G05            4.94      0.14   bcdefgh 0.23 4.72 5.17
    49      G49            4.95      0.14   bcdefgh 0.23 4.72 5.17
    34      G34            5.01      0.14   bcdefgh 0.23 4.78 5.23
    25      G25            5.02      0.14   bcdefgh 0.23 4.79 5.24
    30      G30            5.04      0.14  bcdefghi 0.23 4.82 5.27
    21      G21            5.05      0.14  bcdefghi 0.23 4.82 5.27
    2       G02            5.06      0.14  bcdefghi 0.23 4.84 5.29
    15      G15            5.07      0.14  bcdefghi 0.23 4.85 5.30
    41      G41            5.09      0.14  bcdefghi 0.23 4.86 5.31
    8       G08            5.09      0.14  bcdefghi 0.23 4.86 5.32
    7       G07            5.10      0.14  bcdefghi 0.23 4.87 5.32
    45      G45            5.11      0.14  bcdefghi 0.23 4.88 5.33
    6       G06            5.13      0.14  bcdefghi 0.23 4.90 5.35
    55      G55            5.14      0.14  bcdefghi 0.23 4.92 5.37
    32      G32            5.16      0.14 bcdefghij 0.23 4.93 5.38
    11      G11            5.17      0.14 bcdefghij 0.23 4.94 5.39
    39      G39            5.18      0.14 bcdefghij 0.23 4.95 5.40
    61      G61            5.20      0.14 bcdefghij 0.23 4.97 5.42
    53      G53            5.21      0.14 bcdefghij 0.23 4.98 5.43
    37      G37            5.21      0.14 bcdefghij 0.23 4.99 5.44
    26      G26            5.21      0.14 bcdefghij 0.23 4.99 5.44
    43      G43            5.22      0.14 bcdefghij 0.23 4.99 5.44
    35      G35            5.22      0.14 bcdefghij 0.23 4.99 5.44
    66      G66            5.22      0.14 bcdefghij 0.23 4.99 5.44
    16      G16            5.23      0.14  cdefghij 0.23 5.00 5.45
    68      G68            5.25      0.14  cdefghij 0.23 5.02 5.48
    46      G46            5.25      0.14  cdefghij 0.23 5.03 5.48
    29      G29            5.26      0.14  cdefghij 0.23 5.04 5.49
    47      G47            5.27      0.14  cdefghij 0.23 5.05 5.50
    44      G44            5.29      0.14  cdefghij 0.23 5.06 5.52
    23      G23            5.29      0.14  cdefghij 0.23 5.07 5.52
    56      G56            5.30      0.14  cdefghij 0.23 5.07 5.52
    58      G58            5.31      0.14  cdefghij 0.23 5.08 5.53
    57      G57            5.32      0.14  cdefghij 0.23 5.09 5.54
    63      G63            5.32      0.14  cdefghij 0.23 5.09 5.54
    62      G62            5.32      0.14  cdefghij 0.23 5.09 5.55
    54      G54            5.33      0.14  cdefghij 0.23 5.11 5.56
    51      G51            5.36      0.14  cdefghij 0.23 5.13 5.58
    67      G67            5.37      0.14  cdefghij 0.23 5.14 5.59
    42      G42            5.37      0.14  cdefghij 0.23 5.14 5.60
    22      G22            5.37      0.14  cdefghij 0.23 5.14 5.60
    59      G59            5.37      0.14  cdefghij 0.23 5.14 5.60
    65      G65            5.38      0.14  cdefghij 0.23 5.15 5.60
    69      G69            5.38      0.14  cdefghij 0.23 5.16 5.61
    38      G38            5.38      0.14  cdefghij 0.23 5.16 5.61
    12      G12            5.39      0.14   defghij 0.23 5.17 5.62
    31      G31            5.41      0.14    efghij 0.23 5.19 5.64
    50      G50            5.42      0.14    efghij 0.23 5.20 5.65
    17      G17            5.43      0.14    efghij 0.23 5.20 5.65
    52      G52            5.43      0.14    efghij 0.23 5.20 5.65
    64      G64            5.44      0.14     fghij 0.23 5.21 5.67
    13      G13            5.45      0.14      ghij 0.23 5.22 5.67
    70      G70            5.45      0.14      ghij 0.23 5.23 5.68
    9       G09            5.46      0.14      ghij 0.23 5.23 5.68
    60      G60            5.48      0.14      ghij 0.23 5.25 5.71
    27      G27            5.49      0.14     ghijk 0.23 5.26 5.72
    18      G18            5.50      0.14     ghijk 0.23 5.27 5.72
    36      G36            5.51      0.14      hijk 0.23 5.28 5.73
    48      G48            5.51      0.14      hijk 0.23 5.29 5.74
    40      G40            5.53      0.14      hijk 0.23 5.30 5.75
    33      G33            5.61      0.14       ijk 0.23 5.38 5.83
    19      G19            5.73      0.14        jk 0.23 5.51 5.96
    3       G03            6.08      0.14         k 0.23 5.85 6.30

---

      Fungicide predicted.value std.error groups   ci  low   up
    2        F2            4.97       0.1      a 0.25 4.72 5.21
    1        F1            5.51       0.1      b 0.25 5.27 5.76

# example 6 works

                Df denDF   F.inc           Pr
    (Intercept)  1   6.9 188.100 2.807176e-06
    Treatment   20  27.6   2.963 4.353194e-03

---

                          component  std.error  z.ratio bound %ch
    Block              2.864157e-07         NA       NA     B 0.0
    Column:Row!R       1.790098e-01 0.04842597 3.696565     P 0.0
    Column:Row!Row!cor 5.407771e-01 0.13654557 3.960414     U 0.1

---

            Treatment predicted.value std.error groups   ci  low   up
    7  Hoegrass_0.75L            0.92      0.20      a 0.35 0.57 1.27
    20  Wildcat_300mL            0.94      0.20      a 0.35 0.59 1.28
    4  Atlantis_300mL            0.94      0.21     ab 0.35 0.58 1.29
    21  Wildcat_350mL            0.94      0.20      a 0.35 0.60 1.29
    14     Topik_50mL            0.95      0.20      a 0.35 0.60 1.29
    2    Achieve_300g            0.96      0.21     ab 0.35 0.61 1.32
    6       Control_0            0.98      0.21      a 0.35 0.63 1.34
    8   Hoegrass_1.0L            1.06      0.20     ab 0.35 0.72 1.41
    17   Tristar_1.0L            1.07      0.20     ab 0.35 0.72 1.41
    3    Achieve_380g            1.08      0.20     ab 0.35 0.74 1.43
    5  Atlantis_330mL            1.18      0.20     ab 0.35 0.84 1.53
    16     Topik_85mL            1.22      0.21     ab 0.35 0.87 1.57
    12 MatavenL_2.25L            1.23      0.21     ab 0.35 0.88 1.58
    9   Hoegrass_1.2L            1.30      0.21     ab 0.35 0.95 1.65
    15     Topik_65mL            1.37      0.20     ab 0.35 1.02 1.71
    19  Wildcat_250mL            1.53      0.20     ab 0.35 1.19 1.88
    13  MatavenL_3.0L            1.59      0.21     ab 0.35 1.23 1.94
    11    Hussar_200g            1.66      0.20     ab 0.34 1.31 2.00
    1    Achieve_250g            1.68      0.21     ab 0.35 1.32 2.03
    18   Tristar_1.5L            1.90      0.21     ab 0.35 1.55 2.25
    10    Hussar_150g            2.01      0.21      b 0.35 1.66 2.36

# example 7 works

                   Df denDF     F.inc           Pr
    (Intercept)     1   6.9 188.10000 2.807176e-06
    Control         1  30.1   3.40000 7.504557e-02
    Herbicide       7  27.5   3.41500 9.356669e-03
    Rate            2  29.6   0.03766 9.630875e-01
    Herbicide:Rate 10  29.4   3.18000 7.021822e-03

---

                       component std.error z.ratio bound %ch
    Block                2.9e-07        NA      NA     B 0.0
    Column:Row!R         1.8e-01     0.048     3.7     P 0.0
    Column:Row!Row!cor   5.4e-01     0.137     4.0     U 0.1

---

       Herbicide    Rate predicted.value std.error groups   ci  low   up
    8   Hoegrass     Low            0.92      0.20      a 0.35 0.57 1.27
    21   Wildcat  Medium            0.94      0.20      a 0.35 0.59 1.28
    5   Atlantis     Low            0.94      0.21     ab 0.35 0.59 1.29
    19   Wildcat    High            0.94      0.20      a 0.35 0.60 1.29
    15     Topik     Low            0.95      0.20      a 0.34 0.60 1.29
    3    Achieve  Medium            0.96      0.21     ab 0.35 0.61 1.31
    6    Control Control            0.98      0.21      a 0.35 0.63 1.34
    9   Hoegrass  Medium            1.06      0.20     ab 0.35 0.72 1.41
    18   Tristar     Low            1.07      0.20     ab 0.35 0.72 1.41
    1    Achieve    High            1.08      0.20     ab 0.35 0.74 1.43
    4   Atlantis    High            1.18      0.20     ab 0.34 0.84 1.53
    14     Topik    High            1.22      0.21     ab 0.35 0.87 1.57
    13  MatavenL     Low            1.23      0.21     ab 0.35 0.88 1.58
    7   Hoegrass    High            1.30      0.21     ab 0.35 0.95 1.65
    16     Topik  Medium            1.37      0.20     ab 0.35 1.02 1.71
    20   Wildcat     Low            1.53      0.20     ab 0.35 1.19 1.88
    12  MatavenL    High            1.59      0.21     ab 0.35 1.23 1.94
    10    Hussar    High            1.66      0.20     ab 0.34 1.31 2.00
    2    Achieve     Low            1.68      0.21     ab 0.35 1.32 2.03
    17   Tristar    High            1.90      0.21     ab 0.35 1.55 2.25
    11    Hussar     Low            2.01      0.21      b 0.35 1.66 2.36
    
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

        Variety predicted.value std.error df groups      ci     low      up
    8      Lang         1.97333   0.11779 24      a 0.20152 1.77181 2.17486
    4  Drysdale         2.13000   0.11779 24      a 0.20152 1.92848 2.33152
    11    Wylah         2.13000   0.11779 24      a 0.20152 1.92848 2.33152
    2    Baxter         2.14000   0.11779 24      a 0.20152 1.93848 2.34152
    7      Janz         2.19333   0.11779 24     ab 0.20152 1.99181 2.39486
    5    Endure         2.24000   0.11779 24     ab 0.20152 2.03848 2.44152
    9     Orion         2.27000   0.11779 24     ab 0.20152 2.06848 2.47152
    12    Zippy         2.28333   0.11779 24     ab 0.20152 2.08181 2.48486
    6   Fortune         2.52667   0.11779 24     ab 0.20152 2.32514 2.72819
    3   Caryina         2.54000   0.11779 24     ab 0.20152 2.33848 2.74152
    10  Pugsley         2.75000   0.11779 24      b 0.20152 2.54848 2.95152
    1    Arrino         2.75333   0.11779 24      b 0.20152 2.55181 2.95486

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
    6       Pharoah            2.86      0.42 24      a 0.72 2.14 3.58
    5       Phantom            3.08      0.42 24     ab 0.72 2.36 3.80
    2      Hercules            4.70      0.42 24     ab 0.72 3.98 5.42
    3  Melitopolski            4.78      0.42 24      b 0.72 4.06 5.50
    4     Orangeglo            4.96      0.42 24      b 0.72 4.24 5.68
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
    2      Root           31.61       1.4 18      a 2.42 29.18 34.03
    3      Seed           35.98       1.4 18     ab 2.42 33.55 38.40
    5     Stalk           38.95       1.4 18     bc 2.42 36.53 41.38
    1    Damage           43.52       1.4 18     cd 2.42 41.09 45.94
    4      Silk           48.12       1.4 18      d 2.42 45.69 50.54

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
    3        T4           16.01       1.6 10      a 2.89 13.12 18.90
    4        T8           17.51       1.6 10     ab 2.89 14.62 20.40
    2       T12           21.40       1.6 10     ab 2.89 18.51 24.29
    1        T0           24.39       1.6 10      b 2.89 21.50 27.28

# exercise 7 works

                Df denDF F.inc           Pr
    (Intercept)  1     4 65.47 1.267962e-03
    Variety      6    24 25.34 2.868344e-09

---

            Variety predicted.value std.error groups   ci  low    up
    1 CarolinaCross            2.84      0.69      a 1.18 1.66  4.02
    6       Pharoah            2.86      0.69      a 1.18 1.68  4.04
    5       Phantom            3.08      0.69     ab 1.18 1.90  4.26
    2      Hercules            4.70      0.69     ab 1.18 3.52  5.88
    3  Melitopolski            4.78      0.69      b 1.18 3.60  5.96
    4     Orangeglo            4.96      0.69      b 1.18 3.78  6.14
    7      Sudanese            8.88      0.69      c 1.18 7.70 10.06

# exercise 8 works

                Df denDF   F.inc           Pr
    (Intercept)  1     3 910.800 7.990756e-05
    SeedingRate  5    15   1.321 3.075801e-01

# exercise 9 works

                Df denDF  F.inc           Pr
    (Intercept)  1    20 4333.0 7.399942e-25
    Treatment    4    20   22.8 3.258972e-07

---

      Treatment predicted.value std.error groups   ci   low    up
    2      Root           31.61      1.35      a 2.32 29.29 33.93
    3      Seed           35.98      1.35     ab 2.32 33.66 38.30
    5     Stalk           38.95      1.35     bc 2.32 36.63 41.27
    1    Damage           43.52      1.35     cd 2.32 41.20 45.84
    4      Silk           48.12      1.35      d 2.32 45.80 50.44

# exercise 10 works

                Df denDF   F.inc         Pr
    (Intercept)  1   0.7 124.100 0.11595886
    Treatment    3  10.0   5.659 0.01573632

---

      Treatment predicted.value std.error groups   ci   low    up
    3        T4           16.01       1.6      a 2.89 13.12 18.90
    4        T8           17.51       1.6     ab 2.89 14.62 20.40
    2       T12           21.40       1.6     ab 2.89 18.51 24.29
    1        T0           24.39       1.6      b 2.89 21.50 27.28

# exercise 11 works

                      Df denDF    F.inc           Pr
    (Intercept)        1   5.0 245.1000 1.931825e-05
    Genotype           2  54.3   3.2210 4.765853e-02
    Nitrogen           3  43.2  27.0500 5.270025e-10
    Genotype:Nitrogen  6  50.1   0.2233 9.674091e-01

---

        Genotype predicted.value std.error groups    ci   low     up
    3    Victory           97.68      7.15      a 11.97 85.71 109.65
    1 GoldenRain          104.89      7.15     ab 11.97 92.92 116.86
    2 Marvellous          109.35      7.16      b 11.98 97.37 121.33

---

      Nitrogen predicted.value std.error groups   ci    low     up
    1        0           79.39      7.37      a 12.4  66.99  91.78
    2      0.2           98.89      7.37      b 12.4  86.49 111.28
    3      0.4          114.22      7.37      c 12.4 101.83 126.62
    4      0.6          123.39      7.37      c 12.4 110.99 135.78

# exercise 12 works

                       Df denDF  F.inc          Pr
    (Intercept)         1     2 48.000 0.020205049
    Variety             4    16  6.943 0.001937025
    Irrigation          1     2 22.820 0.041135284
    Variety:Irrigation  4    16  3.994 0.019643763

---

       Variety Irrigation predicted.value std.error groups   ci  low   up
    8  Thumper    Rainfed            4.61      0.99      a 1.73 2.88 6.34
    4  Cobbler    Rainfed            5.47      0.99     ab 1.73 3.74 7.20
    2    Bravo    Rainfed            5.91      0.99      b 1.73 4.18 7.64
    6    Hyola    Rainfed            6.19      0.99     bc 1.73 4.46 7.92
    10 Victory    Rainfed            6.43      0.99     bc 1.73 4.70 8.16
    7  Thumper  Irrigated            6.92      0.99     bc 1.73 5.19 8.65
    9  Victory  Irrigated            7.02      0.99     bc 1.73 5.29 8.75
    1    Bravo  Irrigated            7.68      0.99      c 1.73 5.95 9.41
    5    Hyola  Irrigated            7.70      0.99      c 1.73 5.97 9.43
    3  Cobbler  Irrigated            7.75      0.99      c 1.73 6.02 9.48

# exercise 13 works

                      Df denDF  F.inc       Pr
    (Intercept)        1   5.0 254.40 1.83e-05
    Genotype           2  53.6   4.63 1.39e-02
    Nitrogen           3  21.3  33.70 2.86e-08
    Genotype:Nitrogen  6  50.3   0.30 9.34e-01

---

        Genotype predicted.value std.error groups    ci   low     up
    3    Victory           97.41      7.04      a 11.79 85.62 109.20
    1 GoldenRain          104.31      7.05     ab 11.81 92.50 116.12
    2 Marvellous          110.07      7.04      b 11.79 98.28 121.86

---

      Nitrogen predicted.value std.error groups    ci    low     up
    1        0           79.44      7.11      a 12.23  67.21  91.67
    2      0.2           98.82      7.11      b 12.23  86.58 111.05
    3      0.4          114.08      7.11      c 12.23 101.85 126.32
    4      0.6          123.37      7.10      c 12.21 111.16 135.58

# exercise 14 works

                Df denDF     F.inc           Pr
    (Intercept)  1  17.7 23710.000 3.999662e-29
    Genotype    49  69.4     5.089 4.795863e-10

---

       Genotype predicted.value std.error   groups   ci  low   up
    36      G36            3.68      0.09        a 0.15 3.53 3.83
    28      G28            3.79      0.09       ab 0.15 3.64 3.94
    30      G30            3.90      0.09      abc 0.15 3.75 4.05
    46      G46            3.91      0.09     abcd 0.15 3.76 4.05
    23      G23            3.92      0.09    abcde 0.15 3.77 4.07
    29      G29            3.96      0.09   abcdef 0.15 3.81 4.10
    7       G07            3.96      0.09   abcdef 0.15 3.81 4.11
    21      G21            3.98      0.09  abcdefg 0.15 3.83 4.13
    45      G45            3.98      0.09  abcdefg 0.15 3.83 4.13
    33      G33            4.02      0.09 abcdefgh 0.15 3.87 4.17
    49      G49            4.03      0.09 abcdefgh 0.15 3.89 4.18
    6       G06            4.04      0.09 abcdefgh 0.15 3.89 4.18
    38      G38            4.06      0.09 abcdefgh 0.15 3.91 4.21
    27      G27            4.06      0.09 abcdefgh 0.15 3.91 4.21
    43      G43            4.06      0.09 abcdefgh 0.15 3.92 4.21
    19      G19            4.07      0.09 abcdefgh 0.15 3.92 4.22
    15      G15            4.12      0.09 abcdefgh 0.15 3.98 4.27
    31      G31            4.13      0.09 abcdefgh 0.15 3.98 4.28
    24      G24            4.13      0.09 abcdefgh 0.15 3.98 4.28
    48      G48            4.15      0.09 abcdefgh 0.15 4.00 4.30
    34      G34            4.15      0.09 abcdefgh 0.15 4.00 4.30
    5       G05            4.15      0.09 abcdefgh 0.15 4.00 4.30
    18      G18            4.16      0.09 abcdefgh 0.15 4.01 4.31
    37      G37            4.16      0.09  bcdefgh 0.15 4.01 4.31
    47      G47            4.18      0.09 abcdefgh 0.15 4.03 4.33
    8       G08            4.18      0.09 abcdefgh 0.15 4.03 4.33
    25      G25            4.19      0.09  bcdefgh 0.15 4.05 4.34
    26      G26            4.19      0.09 abcdefgh 0.15 4.05 4.34
    13      G13            4.20      0.09  bcdefgh 0.15 4.05 4.34
    35      G35            4.21      0.09  bcdefgh 0.15 4.06 4.35
    44      G44            4.22      0.09  bcdefgh 0.15 4.07 4.37
    39      G39            4.23      0.09  bcdefgh 0.15 4.08 4.38
    14      G14            4.26      0.09  bcdefgh 0.15 4.11 4.41
    41      G41            4.27      0.09  bcdefgh 0.15 4.12 4.42
    1       G01            4.31      0.09  bcdefgh 0.16 4.15 4.46
    10      G10            4.31      0.09   cdefgh 0.15 4.16 4.46
    3       G03            4.33      0.09   cdefgh 0.15 4.18 4.47
    16      G16            4.33      0.09   cdefgh 0.15 4.18 4.48
    32      G32            4.33      0.09   cdefgh 0.15 4.18 4.48
    22      G22            4.34      0.09   cdefgh 0.15 4.19 4.49
    20      G20            4.36      0.09   cdefgh 0.15 4.21 4.51
    2       G02            4.36      0.09   cdefgh 0.15 4.22 4.51
    40      G40            4.38      0.09   cdefgh 0.15 4.22 4.53
    11      G11            4.38      0.09   cdefgh 0.15 4.23 4.54
    42      G42            4.41      0.09    defgh 0.15 4.27 4.56
    9       G09            4.42      0.09     efgh 0.15 4.27 4.57
    50      G50            4.43      0.09     efgh 0.15 4.28 4.58
    17      G17            4.43      0.09      fgh 0.15 4.29 4.58
    12      G12            4.49      0.09       gh 0.15 4.33 4.64
    4       G04            4.52      0.09        h 0.15 4.37 4.67

# exercise 15 works

                Df denDF   F.inc           Pr
    (Intercept)  1   4.7 433.300 8.380984e-06
    Control      1  15.2  17.600 7.572637e-04
    Season       1  16.1   7.289 1.570471e-02
    Rate         2  15.0   6.489 9.355453e-03
    Season:Rate  2  20.8   1.352 2.804897e-01

---

      Control predicted.value std.error groups   ci  low   up
    1      No            2.37      0.13      a 0.22 2.14 2.59
    2     Yes            3.06      0.17      b 0.30 2.76 3.36

---

      Rate predicted.value std.error groups   ci  low   up
    2   12            1.99      0.17      a 0.29 1.70 2.29
    3    3            2.48      0.17     ab 0.29 2.18 2.77
    4    6            2.62      0.17     bc 0.30 2.33 2.92
    1    0            3.06      0.17      c 0.30 2.76 3.36

---

      Season predicted.value std.error groups   ci  low   up
    1      F            2.14      0.15      a 0.26 1.88 2.40
    3      S            2.59      0.15      b 0.27 2.33 2.86
    2      O            3.06      0.17      c 0.30 2.76 3.36

