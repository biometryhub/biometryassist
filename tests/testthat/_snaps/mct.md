# save produces output

         Species predicted.value std.error  Df groups   ci  low   up
    1     setosa            0.25      0.03 147      a 0.05 0.20 0.29
    2 versicolor            1.33      0.03 147      b 0.05 1.28 1.37
    3  virginica            2.03      0.03 147      c 0.05 1.98 2.07

# mct handles aliased results in asreml with a warning

    Aliased level is: 0.6_cwt:Victory 
    
       Nitrogen     Variety predicted.value std.error groups    ci    low     up
    11    0_cwt     Victory           70.85      8.62     ab 14.57  56.29  85.42
    9     0_cwt Golden_rain           76.58      8.62      a 14.55  62.02  91.13
    10    0_cwt  Marvellous           85.86      8.61    abc 14.54  71.32 100.41
    3   0.2_cwt     Victory           92.22      8.60   abcd 14.53  77.70 106.75
    1   0.2_cwt Golden_rain           99.91      8.67   bcde 14.65  85.26 114.55
    2   0.2_cwt  Marvellous          108.32      8.57    def 14.47  93.85 122.79
    6   0.4_cwt     Victory          113.10      8.63   cdef 14.58  98.52 127.68
    5   0.4_cwt  Marvellous          113.50      8.68    def 14.66  98.83 128.16
    4   0.4_cwt Golden_rain          116.63      8.67   cdef 14.65 101.99 131.28
    7   0.6_cwt Golden_rain          123.75      8.67      f 14.65 109.10 138.40
    8   0.6_cwt  Marvellous          127.53      8.64     ef 14.59 112.94 142.13

# 3 way interaction works

     [1]  99.08  99.26  99.57  99.73  99.73  99.73  99.77  99.79  99.90  99.94
    [11] 100.01 100.04 100.07 100.19 100.21 100.22 100.31 100.32 100.37 100.46
    [21] 100.48 100.48 100.49 100.64 100.68 100.72 101.23

# plots are produced when requested

    

