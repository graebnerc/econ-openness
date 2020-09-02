get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_heatmap <- function(data_used, cor_method_used, text_size_coefs=4, axis_text_size=10, rounding_val=1, upper_only=F){
  cor_data <- round(cor(dplyr::select(data_used, -Country, -Year), use = "pairwise.complete.obs", method = cor_method_used), 2)
  
  if(upper_only){
    final_heatmap <- ggplot(melt(get_lower_tri(cor_data), na.rm = TRUE), aes(Var2, Var1, fill = value)) 
  } else {
    final_heatmap <- ggplot(melt(cor_data), aes(Var2, Var1, fill = value))
  }
  
  final_heatmap <- final_heatmap +
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "#ff0000", high = "#006600", mid = "white", 
                         midpoint = 0, limit = c(-0.25,1), space = "Lab", 
                         name=paste(toupper(substring(cor_method_used, 1,1)), substring(cor_method_used, 2), sep="", collapse=" ")) +
    geom_text(aes(Var2, Var1, label = round(value, rounding_val)), color = "black", size = text_size_coefs) +
    theme_minimal()
  
  if(upper_only){
    final_heatmap <- final_heatmap +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = axis_text_size, hjust = 1),
            axis.text.y = element_text(angle = 0, vjust = 1, 
                                       size = axis_text_size, hjust = 1),
            axis.title = element_blank(), 
            panel.grid.major.y = element_blank(),
            legend.justification = c(1, 0), 
            legend.box.background = element_rect(fill = "white", linetype = "blank"),
            legend.position = c(0.98, 0.025)) +
      coord_fixed()
  } else {
    final_heatmap <- final_heatmap +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = axis_text_size, hjust = 1),
            axis.text.y = element_text(angle = 0, vjust = 1, 
                                       size = axis_text_size, hjust = 1),
            axis.title = element_blank(), panel.grid = element_blank())+
      coord_fixed()
  }
  return(final_heatmap)
}
