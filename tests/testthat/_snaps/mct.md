# Testing asreml predictions

      Nitrogen predicted.value std.error groups    ci    low     up
    1    0_cwt           77.76      6.93      a 14.02  63.74  91.79
    2  0.2_cwt          100.15      6.90      b 13.96  86.20 114.11
    3  0.4_cwt          114.41      6.89      c 13.94 100.47 128.35
    4  0.6_cwt          123.23      6.91      d 13.99 109.24 137.22

---

      Nitrogen predicted.value std.error groups    ci    low     up
    1    0_cwt           77.76      6.93      a 14.02  63.74  91.79
    2  0.2_cwt          100.15      6.90      b 13.96  86.20 114.11
    3  0.4_cwt          114.41      6.89      c 13.94 100.47 128.35
    4  0.6_cwt          123.23      6.91      d 13.99 109.24 137.22

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
    3     0_cwt  Marvellous           86.25      8.80     bc 17.90  68.35 104.16
    4   0.2_cwt     Victory           92.75      8.79      c 17.90  74.86 110.65
    5   0.2_cwt  Marvellous          108.72      8.73      d 17.77  90.94 126.49
    6   0.4_cwt     Victory          111.78      8.86     de 18.04  93.74 129.83
    7   0.4_cwt  Marvellous          113.38      8.87     de 18.05  95.33 131.43
    8   0.4_cwt Golden_rain          115.33      8.94     de 18.20  97.13 133.53
    9   0.6_cwt     Victory          119.64      8.84    def 18.00 101.63 137.64
    10  0.6_cwt Golden_rain          121.48      8.92     ef 18.16 103.32 139.64
    11  0.6_cwt  Marvellous          128.10      8.77      f 17.85 110.25 145.95
    
    Aliased level is: 0.2_cwt:Golden_rain 

---

       Nitrogen     Variety predicted.value std.error groups    ci    low     up
    1     0_cwt     Victory           70.17      9.28      a 18.90  51.27  89.08
    2     0_cwt Golden_rain           72.86      9.22      a 18.78  54.08  91.64
    3     0_cwt  Marvellous           88.36      9.25      b 18.85  69.51 107.21
    4   0.2_cwt  Marvellous          108.91      9.13      c 18.59  90.31 127.50
    5   0.4_cwt     Victory          109.53      9.31      c 18.96  90.57 128.49
    6   0.4_cwt Golden_rain          112.61      9.40      c 19.14  93.47 131.76
    7   0.6_cwt Golden_rain          115.99      9.20      c 18.75  97.24 134.74
    8   0.4_cwt  Marvellous          116.22      9.37      c 19.08  97.13 135.30
    9   0.6_cwt     Victory          116.85      9.34      c 19.03  97.81 135.88
    10  0.6_cwt  Marvellous          128.99      9.20      d 18.74 110.25 147.73
    
    Aliased levels are: 0.2_cwt:Golden_rain and 0.2_cwt:Victory 

