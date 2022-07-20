# save produces output

         Species predicted.value std.error  Df groups   ci  low   up
    1     setosa            0.25      0.03 147      a 0.05 0.20 0.29
    2 versicolor            1.33      0.03 147      b 0.05 1.28 1.37
    3  virginica            2.03      0.03 147      c 0.05 1.98 2.07

# mct removes aliased treatments in aov

    [1] 15.81 25.95 31.75

# mct handles aliased results in asreml with a warning

       Nitrogen     Variety predicted.value std.error groups    ci    low     up
    11    0_cwt     Victory           70.69      8.80      a 14.91  55.78  85.59
    9     0_cwt Golden_rain           77.41      8.82     ab 14.94  62.47  92.34
    10    0_cwt  Marvellous           86.25      8.80    abc 14.89  71.36 101.14
    2   0.2_cwt     Victory           92.75      8.79    bcd 14.89  77.87 107.64
    1   0.2_cwt  Marvellous          108.72      8.73     de 14.78  93.94 123.50
    5   0.4_cwt     Victory          111.78      8.86    cde 15.01  96.77 126.79
    4   0.4_cwt  Marvellous          113.38      8.87     de 15.01  98.37 128.39
    3   0.4_cwt Golden_rain          115.33      8.94    cde 15.14 100.20 130.47
    8   0.6_cwt     Victory          119.64      8.84      e 14.97 104.66 134.61
    6   0.6_cwt Golden_rain          121.48      8.92     de 15.10 106.38 136.59
    7   0.6_cwt  Marvellous          128.10      8.77      e 14.85 113.25 142.94
    
    Aliased level is: 0.2_cwt:Golden_rain 

# 3 way interaction works

     [1]  99.08  99.26  99.57  99.73  99.73  99.73  99.77  99.79  99.90  99.94
    [11] 100.01 100.04 100.07 100.19 100.21 100.22 100.31 100.32 100.37 100.46
    [21] 100.48 100.48 100.49 100.64 100.68 100.72 101.23

# plots are produced when requested

    

