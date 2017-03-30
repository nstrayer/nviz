# Draws a lorrelogram when provided three columns,
# id = patient or cluster ids
# time = time of measurement
# y = binary outcome
#title = title for the plot produced.

lorelogram <- function(id, time, y, title){
  data <- data_frame(id,time,y)

  #function that innumerates all combinations of a given time
  #and all futures times for an individual and then
  #returns a dataframe of time differences and results at both time points
  calc_time_deltas <- function(ind_data){
    results <- expand.grid(ind_data$time,ind_data$time) %>%
      rename(time1 = Var1, time2 = Var2) %>%  #rename columns to meaningful stuff
      right_join(ind_data[,c("time", "y")], by = c("time1" = "time")) %>%
      rename(y1 = y) %>%  #add results for time1 from the full data
      right_join(ind_data[,c("time", "y")], by = c("time2" = "time")) %>%
      rename(y2 = y) %>%  #add results for time2 from the full data.
      mutate(time_diff = time1 - time2, id = ind_data$id[1]) %>% #find difference in times
      filter(time_diff > 0) %>%  #only interested in positive times.
      select(time_diff, y1, y2)  #cleanup output.
  }

  Z <- data %>%
    group_by(id) %>% #grab an individuals data in isolation
    do(calc_time_deltas(.)) #run the immumeration function

  #Predict past outcomes from future by utilizing time differences
  outcome_model <- glm(y1 ~ y2:factor(time_diff), data=Z, family=binomial)

  #grab the parameter estimates (ignoring intercept)
  LOR_estimates <- data_frame(
    time_diff = sort(unique(Z$time_diff)),
    point_est = summary(outcome_model)$coef[-1,1],
    std_err = summary(outcome_model)$coef[-1,2] ) %>%
    mutate(lower_bound = point_est - 1.96*std_err,
           upper_bound = point_est + 1.96*std_err)

  #plot it
  ggplot(LOR_estimates, aes(x = time_diff)) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = "95 % CI"),
                alpha = 0.5) +
    geom_line(aes(y = point_est, color = "point estimate")) +
    scale_colour_manual("",values="black")+
    scale_fill_manual("",values="steelblue") +
    labs(x = "time change", y = "Log Odds Ratio", title = title)
}
