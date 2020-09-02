make_plot_data <- function(var_name, grouping){
  
  if (grouping=="ComplexityGroup"){
    sub_data <- openess_complete %>%
      dplyr::select(one_of("Country", "Year", "ComplexityGroup", var_name)) %>%
      dplyr::filter(!is.na(ComplexityGroup) & !is.na(Country) & ComplexityGroup!="")
    
    work_data <- sub_data %>%
      dplyr::select(-one_of("ComplexityGroup", "Country")) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise_all(mean, na.rm=TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(UQ(as.name(var_name))))  %>%
      dplyr::rename(complete=UQ(as.name(var_name)))
    # browser()
    final_data <- sub_data %>%
      dplyr::select(-Country) %>%
      dplyr::group_by(ComplexityGroup, Year) %>%
      dplyr::summarise(value=mean(UQ(as.name(var_name)), na.rm=TRUE)) %>%
      dplyr::ungroup() %>%
      spread_("ComplexityGroup", "value") %>%
      dplyr::left_join(., work_data, by="Year") %>%
      gather(ComplexityGroup, UQ(as.name(var_name)), -UQ(as.name("Year")))
  } else{
    sub_data <- openess_complete %>%
      dplyr::select(one_of("Country", "Year", "IncomeGroup", var_name)) %>%
      dplyr::filter(!is.na(IncomeGroup) & !is.na(Country) & IncomeGroup!="")
    
    work_data <- sub_data %>%
      dplyr::select(-one_of("IncomeGroup", "Country")) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise_all(mean, na.rm=TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(UQ(as.name(var_name))))  %>%
      dplyr::rename(complete=UQ(as.name(var_name)))
    # browser()
    final_data <- sub_data %>%
      dplyr::select(-Country) %>%
      dplyr::group_by(IncomeGroup, Year) %>%
      dplyr::summarise(value=mean(UQ(as.name(var_name)), na.rm=TRUE)) %>%
      dplyr::ungroup() %>%
      spread_("IncomeGroup", "value") %>%
      dplyr::left_join(., work_data, by="Year") %>%
      gather(IncomeGroup, UQ(as.name(var_name)), -UQ(as.name("Year")))
  }
  return(final_data)
}

make_plot <- function(data_for_plot, title_string, y_var_code, x_label, y_label, grouping_used){
  stopifnot(grouping_used %in% c("ComplexityGroup", "IncomeGroup"))
  
  final_plot <- ggplot(data_for_plot) + 
    geom_line(
      aes_string(x="Year", y=y_var_code, color=grouping_used), 
      key_glyph = draw_key_rect
      ) +
    scale_x_continuous(breaks = seq(1960, 2020, 5)) +
    ylab(y_label) + xlab(x_label) +
    ggtitle(title_string) +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
          panel.border = element_blank(), axis.line = element_line(),
          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  if (grouping_used=="ComplexityGroup"){
    final_plot <- final_plot + scale_color_icae(
      labels = c("All countries", "High complexity", 
                 "Low complexity", "Medium complexity"), 
      palette = "mixed")
  } else {
    final_plot <- final_plot + scale_color_icae(
      labels = c("All countries", "High income", "Upper-medium income", 
                 "Lower-medium income", "Low income"), palette = "mixed")
  }
  return(final_plot)
}

get_plot <- function(y_var_code, title_string, y_label, x_label, kind_group){
  plot_data <- make_plot_data(y_var_code, grouping = kind_group)
  plot2return <- make_plot(plot_data, title_string, y_var_code, x_label, y_label, kind_group)
  return(plot2return)
}