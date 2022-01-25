# CRD designs are supported

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "treatments                              3\n"    
    [4] "Residual                                16\n"   
    [5] "=============================================\n"
    [6] "Total                                   19\n"   

# RCBD designs are supported

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "Block stratum                           3\n"    
    [4] "---------------------------------------------\n"
    [5] "treatments                              10\n"   
    [6] "Residual                                30\n"   
    [7] "=============================================\n"
    [8] "Total                                   43\n"   

# LSD designs are supported

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "Row                                     3\n"    
    [4] "Column                                  3\n"    
    [5] "treatments                              3\n"    
    [6] "Residual                                6\n"    
    [7] "=============================================\n"
    [8] "Total                                   15\n"   

# Split plot designs are supported

     [1] "Source of Variation                          df\n"   
     [2] "==================================================\n"
     [3] "Block stratum                                3\n"    
     [4] "--------------------------------------------------\n"
     [5] "Whole plot stratum\n"                                
     [6] "         treatments                          1\n"    
     [7] "Whole plot Residual                          3\n"    
     [8] "==================================================\n"
     [9] "Subplot stratum\n"                                   
    [10] "         sub_treatments                      3\n"    
    [11] "         treatments:sub_treatments           3\n"    
    [12] "         Subplot Residual                   18\n"    
    [13] "==================================================\n"
    [14] "Total                                       31\n"    

# Split plot designs with names are supported

     [1] "Source of Variation                          df\n"   
     [2] "==================================================\n"
     [3] "Block stratum                                3\n"    
     [4] "--------------------------------------------------\n"
     [5] "Whole plot stratum\n"                                
     [6] "         Water                               1\n"    
     [7] "Whole plot Residual                          3\n"    
     [8] "==================================================\n"
     [9] "Subplot stratum\n"                                   
    [10] "         N                                   3\n"    
    [11] "         Water:N                             3\n"    
    [12] "         Subplot Residual                   18\n"    
    [13] "==================================================\n"
    [14] "Total                                       31\n"    

# Crossed CRD designs are supported

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "A                                       2\n"    
    [4] "B                                       1\n"    
    [5] "A:B                                     2\n"    
    [6] "Residual                                12\n"   
    [7] "=============================================\n"
    [8] "Total                                   17\n"   

---

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "N                                       2\n"    
    [4] "Water                                   1\n"    
    [5] "N:Water                                 2\n"    
    [6] "Residual                                12\n"   
    [7] "=============================================\n"
    [8] "Total                                   17\n"   

# Crossed RCBD designs are supported

     [1] "Source of Variation                     df\n"   
     [2] "=============================================\n"
     [3] "Block stratum                           2\n"    
     [4] "---------------------------------------------\n"
     [5] "A                                       2\n"    
     [6] "B                                       1\n"    
     [7] "A:B                                     2\n"    
     [8] "Residual                                10\n"   
     [9] "=============================================\n"
    [10] "Total                                   17\n"   

# Crossed LSD designs are supported

     [1] "Source of Variation                     df\n"   
     [2] "=============================================\n"
     [3] "Row                                     5\n"    
     [4] "Column                                  5\n"    
     [5] "A                                       2\n"    
     [6] "B                                       1\n"    
     [7] "A:B                                     2\n"    
     [8] "Residual                                20\n"   
     [9] "=============================================\n"
    [10] "Total                                   35\n"   

# Nested designs are supported

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "Row                                     6\n"    
    [4] "Column                                  6\n"    
    [5] "treatments                              6\n"    
    [6] "Residual                                30\n"   
    [7] "=============================================\n"
    [8] "Total                                   48\n"   

# 3 way factorial designs are possible

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "A                                       1\n"    
    [4] "B                                       1\n"    
    [5] "C                                       1\n"    
    [6] "A:B:C                                   1\n"    
    [7] "Residual                                19\n"   
    [8] "=============================================\n"
    [9] "Total                                   23\n"   

---

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "X                                       1\n"    
    [4] "Y                                       1\n"    
    [5] "Z                                       1\n"    
    [6] "X:Y:Z                                   1\n"    
    [7] "Residual                                19\n"   
    [8] "=============================================\n"
    [9] "Total                                   23\n"   

---

     [1] "Source of Variation                     df\n"   
     [2] "=============================================\n"
     [3] "Block stratum                           2\n"    
     [4] "---------------------------------------------\n"
     [5] "X                                       1\n"    
     [6] "Y                                       1\n"    
     [7] "Z                                       1\n"    
     [8] "X:Y:Z                                   1\n"    
     [9] "Residual                                17\n"   
    [10] "=============================================\n"
    [11] "Total                                   23\n"   

# Adding names to 3 way factorial designs works

     [1] "Source of Variation                     df\n"   
     [2] "=============================================\n"
     [3] "Block stratum                           2\n"    
     [4] "---------------------------------------------\n"
     [5] "X                                       1\n"    
     [6] "Y                                       1\n"    
     [7] "Z                                       1\n"    
     [8] "X:Y:Z                                   1\n"    
     [9] "Residual                                17\n"   
    [10] "=============================================\n"
    [11] "Total                                   23\n"   

# split plot allows a character vector for factor names

     [1] "Source of Variation                          df\n"   
     [2] "==================================================\n"
     [3] "Block stratum                                3\n"    
     [4] "--------------------------------------------------\n"
     [5] "Whole plot stratum\n"                                
     [6] "         Water                               1\n"    
     [7] "Whole plot Residual                          3\n"    
     [8] "==================================================\n"
     [9] "Subplot stratum\n"                                   
    [10] "         Nitrogen                            3\n"    
    [11] "         Water:Nitrogen                      3\n"    
    [12] "         Subplot Residual                   18\n"    
    [13] "==================================================\n"
    [14] "Total                                       31\n"    

# Colour blind friendly plots work

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "treatments                              10\n"   
    [4] "Residual                                33\n"   
    [5] "=============================================\n"
    [6] "Total                                   43\n"   

---

    [1] "Source of Variation                     df\n"   
    [2] "=============================================\n"
    [3] "Block stratum                           3\n"    
    [4] "---------------------------------------------\n"
    [5] "treatments                              10\n"   
    [6] "Residual                                30\n"   
    [7] "=============================================\n"
    [8] "Total                                   43\n"   

