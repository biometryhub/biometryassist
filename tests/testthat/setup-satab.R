library(agricolae)
# Completely Randomised Design
trt <- c(1, 5, 10, 20)
rep <- 5
outdesign_crd_satab <- design.crd(trt = trt, r = rep, seed = 42)

# Randomised Complete Block Design
trt <- LETTERS[1:11]
rep <- 4
outdesign_rcbd_satab <- design.rcbd(trt = trt, r = rep, seed = 42)

# Latin Square Design
trt <- c("S1", "S2", "S3", "S4")
outdesign_lsd_satab <- design.lsd(trt)

# Factorial Design (Crossed, Completely Randomised)
trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign_crossed_satab <- design.ab(trt, r = rep, design = "crd")

trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign_crossed_rcbd_satab <- design.ab(trt, r = rep, design = "rcbd")

trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign_crossed_lsd_satab <- design.ab(trt, r = rep, design = "lsd")

# Factorial Design (Nested, Latin Square)
trt <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3")
outdesign_nested_satab <- design.lsd(trt)

# Split plot design
trt1 <- c("A", "B")
trt2 <- 1:4
rep <- 4
outdesign_split_satab <- design.split(trt1, trt2, r = rep)
