field_layout <- function(data, row_var, col_var, other_var){

other_var_fac <- is.factor(data[[other_var]])   
        

if(other_var_fac){
tt <- as.data.frame(tapply(as.numeric(data[[other_var]]), list(data[[row_var]], data[[col_var]]),
                   mean, na.rm = TRUE))
newtt <- expand.grid(row = 1:length(rownames(tt)), col = 1:length(names(tt)))
newtt$mean_dat = factor(unlist(tt))
}

else
  
{
  tt <- as.data.frame(tapply(as.numeric(data[[other_var]]), list(data[[row_var]], data[[col_var]]),
                             mean, na.rm = TRUE))
  newtt <- expand.grid(row = 1:length(rownames(tt)), col = 1:length(names(tt)))
  newtt$mean_dat = unlist(tt)
}  

ggplot(data = newtt, aes(y = row, x = col, 
                        fill = mean_dat)) + 
  geom_tile(colour = "black") + 
  theme_bw() + 
  labs(x = col_var, y = row_var, fill = other_var)
  
  
}


field_layout(data = dat, row_var = "Row", col_var = "Column", other_var = "Mainplot")
