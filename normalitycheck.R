data <- data.frame(x = rnorm(n = 1500, mean = 20, sd = 1)^(-3))

histplot <- ggplot2::ggplot(data, ggplot2::aes(x = x)) + 
  ggplot2::geom_histogram(bins = ifelse(nrow(data) < 31, 7, 11), 
                          fill = "aquamarine3", colour = "black") +
  ggplot2::theme_bw() + 
  ggplot2::labs(y = "Frequency", x = "")
histplot  

stest <- shapiro.test(data$x)


power_val <- c(-5, -4, -3, -2, -1, -0.5, 0.5, 2, 3, 4, 5)  

trans_tab <- data.frame(Transformation = c(paste("y^", power_val, sep = ""), "ln(y)"),
pval = NA)

# Trying standard transformations
for(i in 1:length(power_val)){
  newdat <- data$x^(power_val[i])
  trans_tab$pval[i] <- round(shapiro.test(newdat)$p.value,3)
}
newdat <- log(data$x)
trans_tab$pval[trans_tab$Transformation == "ln(y)"] <- round(shapiro.test(newdat)$p.value,3)
trans_tab

trans <- trans_tab$Transformation[which.max(ifelse(trans_tab$pval > 0.05, trans_tab$pval, NA))]

numb_obs <- paste0("Number of observations: ", length(!is.na(data$x)), " \n")


shapiro_text <- paste0("Shapiro-Wilk's test p-value = ", round(stest$p.value,3), ". \n")

shapiro_interp <-
if(stest$p.value > 0.05){"Shapiro-Wilk's test indicates the distribution \n is similar to the normal distribution. \n Transformation not required." } else{
paste0("Shapiro-Wilk's test indicates the distribution \n deviates from the normal distribution. \n", 
       "Try a transformation based on ", 
                     trans, ". \n")
}

res_text <- paste0(numb_obs, shapiro_text, shapiro_interp)

res_text_graph <- ggplot2::ggplot() + 
  ggplot2::annotate("text", x = 4, y = 25, size = 6, label = res_text) + 
  ggplot2::theme_void()

library(cowplot)
plot_grid(histplot, res_text_graph, ncol = 1)


