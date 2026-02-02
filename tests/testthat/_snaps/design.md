# CRD designs are supported

    Source of Variation                     df
    =============================================
    treatments                              3
    Residual                                16
    =============================================
    Total                                   19

# RCBD designs are supported

    Source of Variation                     df
    =============================================
    Block stratum                           3
    ---------------------------------------------
    treatments                              10
    Residual                                30
    =============================================
    Total                                   43

# LSD designs are supported

    Source of Variation                     df
    =============================================
    Row                                     3
    Column                                  3
    treatments                              3
    Residual                                6
    =============================================
    Total                                   15

# Split plot designs are supported

    Source of Variation                          df
    ==================================================
    Block stratum                                3
    --------------------------------------------------
    Whole plot stratum
             treatments                          1
    Whole plot Residual                          3
    ==================================================
    Subplot stratum
             sub_treatments                      3
             treatments:sub_treatments           3
             Subplot Residual                   18
    ==================================================
    Total                                       31

# Split plot designs with names are supported

    Source of Variation                          df
    ==================================================
    Block stratum                                3
    --------------------------------------------------
    Whole plot stratum
             Water                               1
    Whole plot Residual                          3
    ==================================================
    Subplot stratum
             N                                   3
             Water:N                             3
             Subplot Residual                   18
    ==================================================
    Total                                       31

# Crossed CRD designs are supported

    Source of Variation                     df
    =============================================
    A                                       2
    B                                       1
    A:B                                     2
    Residual                                12
    =============================================
    Total                                   17

---

    Source of Variation                     df
    =============================================
    N                                       2
    Water                                   1
    N:Water                                 2
    Residual                                12
    =============================================
    Total                                   17

# Crossed RCBD designs are supported

    Source of Variation                     df
    =============================================
    Block stratum                           2
    ---------------------------------------------
    A                                       2
    B                                       1
    A:B                                     2
    Residual                                10
    =============================================
    Total                                   17

# Crossed LSD designs are supported

    Source of Variation                     df
    =============================================
    Row                                     5
    Column                                  5
    A                                       2
    B                                       1
    A:B                                     2
    Residual                                20
    =============================================
    Total                                   35

# Nested designs are supported

    Source of Variation                     df
    =============================================
    Row                                     6
    Column                                  6
    treatments                              6
    Residual                                30
    =============================================
    Total                                   48

# 3 way factorial designs are possible

    Source of Variation                     df
    =============================================
    A                                       1
    B                                       1
    C                                       1
    A:B:C                                   1
    Residual                                19
    =============================================
    Total                                   23

---

    Source of Variation                     df
    =============================================
    X                                       1
    Y                                       1
    Z                                       1
    X:Y:Z                                   1
    Residual                                19
    =============================================
    Total                                   23

# Adding names to 3 way factorial designs works

    Source of Variation                     df
    =============================================
    Block stratum                           2
    ---------------------------------------------
    X                                       1
    Y                                       1
    Z                                       1
    X:Y:Z                                   1
    Residual                                17
    =============================================
    Total                                   23

# split plot allows a character vector for factor names

    Source of Variation                          df
    ==================================================
    Block stratum                                3
    --------------------------------------------------
    Whole plot stratum
             Water                               1
    Whole plot Residual                          3
    ==================================================
    Subplot stratum
             Nitrogen                            3
             Water:Nitrogen                      3
             Subplot Residual                   18
    ==================================================
    Total                                       31

# Output is produced when quiet = FALSE

    Code
      cat(des$satab)
    Output
      Source of Variation                     df
       =============================================
       treatments                              10
       Residual                                33
       =============================================
       Total                                   43

# Colour blind friendly plots work

    Source of Variation                     df
    =============================================
    treatments                              10
    Residual                                33
    =============================================
    Total                                   43

---

    Source of Variation                     df
    =============================================
    Block stratum                           3
    ---------------------------------------------
    treatments                              10
    Residual                                30
    =============================================
    Total                                   43

