# Takes an alternating logistic regression fit from the ordgee function
# and generates a nice table with plots of the coefficient values and their confidence intervals.

alr_results <- function(model_fit, title){
  model_summary <- summary(model_fit)

  #Grab the mean model estimates and standard errors
  mean_model_params <- model_summary$mean[,c(1,2)] %>%
    mutate(model_type = "mean model", variable = row.names(.))
  #Grab the correlation model's as well
  corr_model_params <- model_summary$correlation[,c(1,2)] %>%
    mutate(model_type = "correlation model", variable = row.names(.))
  #combine the two
  model_params <- rbind(mean_model_params, corr_model_params) %>%
    mutate(model_type = fct_inorder(model_type))

  ggplot(model_params, aes(y = estimate,x = variable, color = variable)) +
    geom_hline(yintercept = 0, alpha = 0.2)+
    geom_pointrange(aes(ymin = estimate - 1.96*san.se, ymax = estimate +1.96*san.se)) +
    geom_text(aes(label = sprintf("Est: %3.3f", estimate)),
              vjust = -1, size = 3)+
    geom_text(aes(label = sprintf("SE: %3.3f", san.se)),
              vjust = 1.8, size = 3)+
    facet_wrap(~model_type, scales = "free_y") +
    coord_flip() +
    scale_colour_brewer(palette="Paired") +
    labs(y = "coefficient estimate", title = title, subtitle = "The 'mean model' panel contains estimates of the effect of a given variable's one unit increase\n(holding all other variables constant) on the log-odds of infection occuring at a given measurement.\n\nThe 'correlation model' panel shows a variable's effect on the log-odds of an infection occuring given\nan infection occured at another measurement.\n")
}
