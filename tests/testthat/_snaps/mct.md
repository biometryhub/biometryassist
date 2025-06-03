# save produces output

         Species predicted.value std.error  df groups   ci  low   up
    1     setosa            0.25      0.03 147      a 0.07 0.18 0.31
    2 versicolor            1.33      0.03 147      b 0.07 1.26 1.39
    3  virginica            2.03      0.03 147      c 0.07 1.96 2.09

# mct removes aliased treatments in aov

    [1] 15.81 25.95 31.75

# mct handles aliased results in asreml with a warning

       Nitrogen     Variety predicted.value std.error groups    ci   low     up
    1     0_cwt     Victory           70.69      8.80      a 30.43 40.26 101.12
    2     0_cwt Golden_rain           77.41      8.82     ab 30.49 46.92 107.90
    3     0_cwt  Marvellous           86.25      8.80    abc 30.40 55.85 116.65
    4   0.2_cwt     Victory           92.75      8.79    bcd 30.39 62.36 123.14
    5   0.2_cwt  Marvellous          108.72      8.73     de 30.18 78.54 138.90
    6   0.4_cwt     Victory          111.78      8.86    cde 30.64 81.14 142.42
    7   0.4_cwt  Marvellous          113.38      8.87     de 30.65 82.73 144.03
    8   0.4_cwt Golden_rain          115.33      8.94    cde 30.90 84.43 146.24
    9   0.6_cwt     Victory          119.64      8.84      e 30.57 89.07 150.21
    10  0.6_cwt Golden_rain          121.48      8.92     de 30.83 90.65 152.32
    11  0.6_cwt  Marvellous          128.10      8.77      e 30.31 97.79 158.41
    
    Aliased level is: 0.2_cwt:Golden_rain 

---

       Nitrogen     Variety predicted.value std.error groups    ci   low     up
    1     0_cwt     Victory           70.17      9.28      a 31.51 38.66 101.68
    2     0_cwt Golden_rain           72.86      9.22      a 31.30 41.55 104.16
    3     0_cwt  Marvellous           88.36      9.25     ab 31.43 56.93 119.78
    4   0.2_cwt  Marvellous          108.91      9.13     bc 30.99 77.91 139.90
    5   0.4_cwt     Victory          109.53      9.31     bc 31.61 77.92 141.14
    6   0.4_cwt Golden_rain          112.61      9.40     bc 31.91 80.71 144.52
    7   0.6_cwt Golden_rain          115.99      9.20      c 31.25 84.74 147.25
    8   0.4_cwt  Marvellous          116.22      9.37      c 31.81 84.40 148.03
    9   0.6_cwt     Victory          116.85      9.34      c 31.73 85.12 148.57
    10  0.6_cwt  Marvellous          128.99      9.20      c 31.24 97.75 160.23
    
    Aliased levels are: 0.2_cwt:Golden_rain and 0.2_cwt:Victory 

# 3 way interaction works

     [1]  99.08  99.26  99.57  99.73  99.73  99.73  99.77  99.79  99.90  99.94
    [11] 100.01 100.04 100.07 100.19 100.21 100.22 100.31 100.32 100.37 100.46
    [21] 100.48 100.48 100.49 100.64 100.68 100.72 101.23

# plots are produced when requested

    

