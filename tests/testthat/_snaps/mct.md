# Testing asreml predictions

      Nitrogen predicted.value std.error groups    ci    low     up
    1    0_cwt           77.76      6.93      a 14.02  63.74  91.79
    2  0.2_cwt          100.15      6.90      b 13.96  86.20 114.11
    3  0.4_cwt          114.41      6.89      c 13.94 100.47 128.35
    4  0.6_cwt          123.23      6.91      c 13.99 109.24 137.22

# save produces output

         Species predicted.value std.error  df groups   ci  low   up
    1     setosa            0.25      0.03 147      a 0.06 0.19 0.30
    2 versicolor            1.33      0.03 147      b 0.06 1.27 1.38
    3  virginica            2.03      0.03 147      c 0.06 1.97 2.08

# mct removes aliased treatments in aov

    [1] 15.81 25.95 31.75

# mct handles aliased results in asreml with a warning

       Nitrogen     Variety predicted.value std.error groups    ci    low     up
    1     0_cwt     Victory           70.69      8.80      a 17.92  52.77  88.61
    2     0_cwt Golden_rain           77.41      8.82     ab 17.96  59.45  95.37
    3     0_cwt  Marvellous           86.25      8.80    abc 17.90  68.35 104.16
    4   0.2_cwt     Victory           92.75      8.79    bcd 17.90  74.86 110.65
    5   0.2_cwt  Marvellous          108.72      8.73     de 17.77  90.94 126.49
    6   0.4_cwt     Victory          111.78      8.86    cde 18.04  93.74 129.83
    7   0.4_cwt  Marvellous          113.38      8.87     de 18.05  95.33 131.43
    8   0.4_cwt Golden_rain          115.33      8.94    cde 18.20  97.13 133.53
    9   0.6_cwt     Victory          119.64      8.84      e 18.00 101.63 137.64
    10  0.6_cwt Golden_rain          121.48      8.92     de 18.16 103.32 139.64
    11  0.6_cwt  Marvellous          128.10      8.77      e 17.85 110.25 145.95
    
    Aliased level is: 0.2_cwt:Golden_rain 

---

       Nitrogen     Variety predicted.value std.error groups    ci    low     up
    1     0_cwt     Victory           70.17      9.28      a 18.90  51.27  89.08
    2     0_cwt Golden_rain           72.86      9.22      a 18.78  54.08  91.64
    3     0_cwt  Marvellous           88.36      9.25     ab 18.85  69.51 107.21
    4   0.2_cwt  Marvellous          108.91      9.13     bc 18.59  90.31 127.50
    5   0.4_cwt     Victory          109.53      9.31     bc 18.96  90.57 128.49
    6   0.4_cwt Golden_rain          112.61      9.40     bc 19.14  93.47 131.76
    7   0.6_cwt Golden_rain          115.99      9.20      c 18.75  97.24 134.74
    8   0.4_cwt  Marvellous          116.22      9.37      c 19.08  97.13 135.30
    9   0.6_cwt     Victory          116.85      9.34      c 19.03  97.81 135.88
    10  0.6_cwt  Marvellous          128.99      9.20      c 18.74 110.25 147.73
    
    Aliased levels are: 0.2_cwt:Golden_rain and 0.2_cwt:Victory 

# 3 way interaction works

     [1]  99.08  99.26  99.57  99.73  99.73  99.73  99.77  99.79  99.90  99.94
    [11] 100.01 100.04 100.07 100.19 100.21 100.22 100.31 100.32 100.37 100.46
    [21] 100.48 100.48 100.49 100.64 100.68 100.72 101.23

# plots are produced when requested

    

