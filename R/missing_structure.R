#takes a dataframe and returns a plot that shows the structure with missing data highlighted.

missing_structure <- function(x){
  x %>%
    is.na() %>%
    as_data_frame() %>%
    mutate(rowNum = 1:dim(.)[1]) %>%
    gather(variable, missing, -rowNum) %>%
    ggplot(aes(x = variable,
               y = rowNum)) +
    geom_raster(aes(fill = missing)) +
    # scale_fill_grey(name = "",
    #                 labels = c("Present","Missing")) +
    scale_fill_manual(values = c("#636363","#f03b20")) +
    theme_minimal() +
    geom_vline(aes(xintercept = which(variable %in% variable) - 0.5), alpha = 0.3) +
    theme(axis.text.x  = element_text(angle=45, hjust = 1, vjust = 1)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
