des.out <- design(type = "crd", treatments = c(1, 5, 10, 20),
                  reps = 5, nrows = 4, ncols = 5, seed = 42, plot = FALSE)

# edge

# des <- des.out$design
des.out$design$row <- des.out$design$row+1
des.out$design$col <- des.out$design$col+1
autoplot(des.out)
buffers <- data.frame(row = c(rep(1, max(des.out$design$row)+1),
                              rep(max(des.out$design$row)+1, max(des.out$design$col)+1),
                              rep(2:max(des.out$design$row), 2)),
                      col = c(rep(1:(max(des.out$design$col)+1), 2),
                              rep(1, max(des.out$design$row)-1),
                              rep(max(des.out$design$col)+1, max(des.out$design$row)-1)),
                      plots = NA, rep = NA, treatments = factor(123))

des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)

# Rows
buffers <- data.frame(row = rep(seq(1, max(des.out$design$row)+1),
                                each = max(des.out$design$col)+1),
                      col = rep(seq(1, max(des.out$design$col)+1),
                                times = max(des.out$design$row)+1),
                      plots = NA, rep = NA, treatments = factor(123))

# Not working
des.out$design <- rbind(des.out$design, buffers)
autoplot(des.out)
