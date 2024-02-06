des.out <- design(type = "crd", treatments = c(1, 5, 10, 20, 25, 30, 35),
                  reps = 4, nrows = 4, ncols = 7, seed = 42, plot = FALSE)

nrow <- max(des.out$design$row)
ncol <- max(des.out$design$col)

# edge

# des <- des.out$design
# des.out$design$row <- des.out$design$row+1
# des.out$design$col <- des.out$design$col+1
autoplot(des.out)
buffers <- data.frame(row = c(rep(1, ncol+2),
                              rep(nrow+2, ncol+2),
                              rep(2:(nrow+1), 2)),
                      col = c(rep(1:(ncol+2), 2),
                              rep(1, nrow),
                              rep(ncol+2, nrow)),
                      plots = NA, rep = NA, treatments = factor(123))
des.out$design$row <- des.out$design$row+1
des.out$design$col <- des.out$design$col+1
des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)

des.out <- design(type = "crd", treatments = c(1, 5, 10, 20, 25, 30, 35),
                  reps = 4, nrows = 4, ncols = 7, seed = 42, plot = FALSE)

# Rows
buffers <- data.frame(row = rep(seq(1, (2*nrow)+1, by = 2),
                                each = ncol),
                      col = rep(seq(1, ncol),
                                times = nrow+1),
                      plots = NA, rep = NA, treatments = factor(123))

des.out$design$row <- 2*des.out$design$row
des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)

des.out <- design(type = "crd", treatments = c(1, 5, 10, 20, 25, 30, 35),
                  reps = 4, nrows = 4, ncols = 7, seed = 42, plot = FALSE)


des.out <- design(type = "crd", treatments = c(1, 5, 10, 20, 25, 30, 35),
                  reps = 4, nrows = 4, ncols = 7, seed = 42, plot = FALSE)

# Cols
buffers <- data.frame(row = rep(seq(1, nrow),
                                times = ncol+1),
                      col = rep(seq(1, (2*ncol)+1, by = 2),
                                each = nrow),
                      plots = NA, rep = NA, treatments = factor("buffer"))

des.out$design$col <- 2*des.out$design$col
des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)


# Double Rows
buffers <- data.frame(row = c(rep(seq(1, (3*nrow)-2, by = 3),
                                  each = ncol),
                              rep(seq(3, (3*nrow), by = 3),
                                  each = ncol)),
                      col = rep(seq(1, ncol),
                                times = 2*nrow),
                      plots = NA, rep = NA, treatments = factor(123))

des.out$design$row <- (3*des.out$design$row)-1
des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)

des.out <- design(type = "crd", treatments = c(1, 5, 10, 20, 25, 30, 35),
                  reps = 4, nrows = 4, ncols = 7, seed = 42, plot = FALSE)

# Double Columns
buffers <- data.frame(row = rep(seq(1, nrow),
                                times = 2*ncol),
                      col = c(rep(seq(1, (3*ncol)-2, by = 3),
                                  each = nrow),
                              rep(seq(3, (3*ncol), by = 3),
                                  each = nrow)),
                      plots = NA, rep = NA, treatments = factor(123))

des.out$design$col <- (3*des.out$design$col)-1
des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)



des.out <- design(type = "rcbd", treatments = c(1, 5, 10, 20, 25, 30, 35, 40),
                  reps = 4, nrows = 4, ncols = 8, brows = 2, bcols = 4,
                  seed = 42, plot = FALSE)

# Blocks

nrow <- max(des.out$design$row)
ncol <- max(des.out$design$col)
nblocks <- max(as.numeric(des.out$design$block))


buffers <- data.frame(row = rep(1:nrow,
                                times = ncol+1),
                      col = rep(seq(1, (2*ncol)+1, by = 2),
                                each = nrow),
                      plots = NA, rep = NA, treatments = factor("buffer"))

des.out$design$col <- 2*des.out$design$col
des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)
