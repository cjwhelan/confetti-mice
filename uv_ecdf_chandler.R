#from Chandler

rm(list = ls())

library(ggplot2)
library(dplyr)


find_ecdf_x <- function(x, x_range, nx=100){
    #' Find X values corresponding to each ecdf y value
    
    # Create ECDF function    
    to_pred_x <- seq(x_range[1], x_range[2], length.out=nx)
    ecdf_fxn <- ecdf(x)
    pred_ecdf <- ecdf_fxn(to_pred_x)

    # Fit spline to predict X for a given ecdf y value
    to_pred_y <- seq(0, 1, length.out=nx)
    sp_fxn <- splinefun(pred_ecdf, to_pred_x, method="monoH.FC") # Creates function that will predict X for any ecdf y-value
    pred_x <- sp_fxn(to_pred_y)
    
    # Store results in dataframe
    pred_df <- data.frame("x"=pred_x, "ecdf_y"=to_pred_y) # Predicted x value for each ecdf bin in to_pred_y

    return(pred_df)

}


uv_df <- read.csv("D:\\confetti_mice\\UV_mice_allmonths_ecdf_sorted.csv", stringsAsFactors=FALSE)
no_uv_df <- read.csv("D:\\confetti_mice\\No_UV_mice_allmonths_ecdf_sorted.csv", stringsAsFactors=FALSE)


var_name <- "structure_volume"
min_x <- min(min(uv_df[, var_name]), min(no_uv_df[, var_name]))
max_x <- max(max(uv_df[, var_name]), max(no_uv_df[, var_name]))
x_range <- c(min_x, max_x)

months <- unique(c(uv_df$month, no_uv_df$month))
res_list <- list()
for(m in months){

    uv_month <- subset(uv_df, month==m)
    no_uv_month <- subset(no_uv_df, month==m)
    
    uv_ecdf_x_df <- find_ecdf_x(uv_month[, var_name], x_range)     
    no_uv_ecdf_x_df <- find_ecdf_x(no_uv_month[, var_name], x_range)

    #Caclulate change in X for each ecdf bin
    temp_uv_ecdf_x_df <- uv_ecdf_x_df %>% dplyr::rename("UV_x" = "x")
    temp_no_uv_ecdf_x_df <- no_uv_ecdf_x_df %>% dplyr::rename("no_UV_x" = "x")
    
    month_pred_ecdf_x_df <- merge(temp_uv_ecdf_x_df, temp_no_uv_ecdf_x_df, by="ecdf_y")
    month_pred_ecdf_x_df$dx <- month_pred_ecdf_x_df$UV_x - month_pred_ecdf_x_df$no_UV_x
    month_pred_ecdf_x_df$month <- m

    res_list[[m]] <- month_pred_ecdf_x_df
    
}


res_df <- do.call(rbind, res_list)
write.csv(res_df, "D:\\confetti_mice\\ecdf_difference.csv", row.names=FALSE)


dx_plot <- ggplot(data=res_df, aes(x=ecdf_y, y=dx, group=month, color=month)) +
    geom_line() +
    ylab(paste("Difference in", var_name))

ggsave("D:\\confetti_mice\\ecdf_difference.png", dx_plot)


# Plot fits to make sure they look like the true ECDF
uv_pred_x_df <- res_df[c("ecdf_y", "UV_x", "month")]
uv_pred_x_df$treatment <- "UV"
uv_pred_x_df <- uv_pred_x_df %>% dplyr::rename("x"="UV_x")

no_uv_pred_x_df <- res_df[c("ecdf_y", "no_UV_x", "month")]
no_uv_pred_x_df$treatment <- "No UV"
no_uv_pred_x_df <- no_uv_pred_x_df %>% dplyr::rename("x"="no_UV_x")

fit_df <- rbind(uv_pred_x_df, no_uv_pred_x_df)
all_data_df <- rbind(uv_df, no_uv_df)
fits_plot <- ggplot(data=all_data_df, aes_string(x=var_name, group="treatment", color="treatment")) +
    stat_ecdf() + # plot actual ecdf as a solid line
    geom_point(data=fit_df, aes(x=x, y=ecdf_y, color=treatment), size=1, shape=1, inherit.aes=FALSE) + # Plot predicted x for each ecdf y, as open circle
    ylab("ECDF") +
    facet_wrap(. ~ month) # make each month a subplot


ggsave("D:\\confetti_mice\\ecdf_with_fits.png", fits_plot)
    


