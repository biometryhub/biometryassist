library(ggplot2)
library(asreml)
library(biometryassist)
library(tidyverse)
library(lubridate)
library(viridis)

theme_set(theme_bw())



datn <- read.csv("C:/Users/sharo/Documents/SNSTATS/Clients/Daniel Menadue/Objective 2 Hart/HartNDVI.csv") 
str(datn) 
datn$Treatment <- factor(datn$Treatment, levels = c("Control", 
    "Balance&Grow", "GABA", "Kelpak", "RemoveFlorets", "Seasol"))

datn <- datn %>% mutate(across(c(1:5), factor))  

datn$Date <- dmy(datn$Date)

datn <- datn %>% arrange(Date, Row, Bay)

datn$facDate <- factor(datn$Date)
datn$plot <- factor(paste(datn$Row, datn$Bay, sep = ""))

dat.asr <- asreml(fixed = NDVI ~ facDate + Variety + Variety:facDate + Treatment +
                    Variety:Treatment + Treatment:facDate + 
                    Variety:Treatment:facDate, 
random = ~ Block, 
residual = ~ dsum(~ ar1(Row):id(Bay)|facDate), data = datn)



dat.asr2d <- asreml(fixed = Biomass ~ Variety + Treatment +
                    Variety:Treatment, 
random = ~ Block, 
residual = ~ ar1(Row):id(Bay), data = dat)
dat.asr2d <- update(dat.asr2d)

Row <- "Row"
Coulmn <- "Bay"
model.obj <- dat.asr2d

#########################################################################

# vario_df <- function(model.obj, Row = NA, Column = NA) {
#     # The 'z' value for the variogram is the residuals
#     # Need to be able to pull out the x/y from the model object

# This works for 2 dimensions 

dims <- unlist(strsplit(names(model.obj$R.param[1]), ":"))

dims <- unlist(strsplit(names(dat.asr2d$R.param[1]), ":"))


# This will flag that a dsum is used in the model - should do this first then 
# do the 2D case  

# now to identify the terms in the residual - the lasst will be the factor 
# associated with the last term in the res.terms vector


# Once dsum is identified - the dat.asr$formulae$residual will need to be broken
# down into it's components. 
# Seperate data set for each |term and row column within each |term

# pulling out the residual formulae as a character
res.call <- as.character(dat.asr$formulae$residual)[2]

# identifying if dsum is present
# if present pull out residual terms
if(any(grepl("dsum", res.call))){
res.terms <- unlist(strsplit(res.call, "[~:| ()]+"))[-1]
asr.terms <- c("id", "ar1", "ar2", "cor")

res.terms <- res.terms[!is.element(res.terms, asr.terms)]
}
res.terms

# Break up the data and the residuals into all levels of grouping factor
# Calculate the gamas for each level and create a separate variogram for each 
# level

# Maybe have the option to combine into a single graph or keep as separate plots


# This is for 2D

    if(missing(Row) | is.na(Row) | is.null(Row)) {
        Row <- as.numeric(model.obj$mf[[dims[1]]])
    }
    if(missing(Column) | is.na(Column) | is.null(Column)) {
        Column <- as.numeric(model.obj$mf[[dims[2]]])
    }

    nrows <- max(Row)
    ncols <- max(Column)

    Resid <- residuals(model.obj)#[order(Column, Row)], nrow = nrows)
    # Resid <- matrix(residuals(model.obj)[order(Column, Row)], nrow = nrows)

    vario <- expand.grid(Row = 0:(nrows-1), Column = 0:(ncols-1))

    # Ignore the 0, 0 case (gamma=0, counted row*cols times)
    gammas <- rep(0, nrows*ncols)
    nps <- rep(nrows*ncols, nrows*ncols)

    for (index in 2:nrow(vario)) {
        i <- vario[index, 'Row']
        j <- vario[index, 'Column']

        gamma <- 0
        np <- 0
        for (val_index in 1:nrow(vario)) {
            # val <- vals[val_index, ]

            # Deliberate double-counting so that offset handling is easy
            # (so e.g. we compute distance from (1,1)->(2,3), and then again
            # later from (2,3)->(1,1)).
            for (offset in unique(list(c(i, j), c(-i, j), c(i, -j), c(-i, -j)))) {
                row <- Row[val_index] + offset[1]
                col <- Column[val_index] + offset[2]

                if (0 < row && row <= nrows && 0 < col && col <= ncols && !is.na(Resid[val_index])) {
                    other <- Resid[Row == row & Column == col]

                    if (!is.na(other)) {
                        gamma <- gamma + (Resid[val_index] - other)^2
                        np <- np + 1
                    }
                }
            }
        }
        # Since we double-counted precisely, halve to get the correct answer.
        np <- np / 2
        gamma <- gamma / 2

        if (np > 0) {
            gamma <- gamma / (2*np)
        }

        gammas[index] <- gamma
        nps[index] <- np
    }
    nps[1] <- nps[1]-sum(is.na(Resid))
    vario <- cbind(vario, data.frame(gamma = gammas, np = nps))
    colnames(vario) <- c(dims, "gamma", "np")
    class(vario) <- c("variogram", "data.frame")
    return(vario)
}


vario_df(dat.asr, Row = "Row", Column = "Bay")


