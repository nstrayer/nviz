#' A more elegant regression output
#'
#' If you want to avoid putting yet another boring table into your report this function will display all the standard coefficient information from a model
#'     but also will allow you to visually display the coefficient results and their coefficient values, allowing easier comparisons and iterpretations of
#'     concepts like effect size. Currently it supports any model who's output can by tidied by the Broom package.
#'
#'
#' @param ... A model fit or fits to be plotted.
#' @param model_or_models Either a single fit or a list of fits. If the list of fits is named then those names are used in plotting. Otheriwse they are just stated as "model 1", ...
#' @param plot_title Main title for the plot.
#' @param zero_line A line at zero on the x-axis. Used to determine statistical significance (if confidence interval overlaps line, coefficient is significant).
#' @param only_in_common If TRUE will only plot the coefficients that appear in all of your models (although for fair comparisons all models should have all coefficients).
#' @param model_interp A sibtitle for the plot that explains how to interpret the results contained in a statistically sound way. Default works for a standard linear regression.
#' @export
#' @examples
#' my_models <- list(model_1,model_2,model_3)
#' names(my_models) <- c("No Interaction", "Wrong Interaction", "Right Interaction")
#' regression_viz(my_models, plot_title = "Comparing Beta estimates"))
regression_viz <- function(model_or_models, plot_title = "", zero_line = TRUE, only_in_common = TRUE, model_interp = "default"){

  #test to see if we have a single model fit or a list of model fits.
  multiple_models <- class(model_or_models) == "list"

  #if supplied multiple models in a list set up the model names
  if (multiple_models){
    model_names <- names(model_or_models)
    if(is.null(model_names)){
      model_names <- paste("model", 1:length(model_or_models))
    }

    #turn each model into its own dataframe using broom's tidy function
    model_tables <- lapply(model_or_models, broom::tidy)

    #for each element in our fits, pull it out and append to a big dataframe with it's name.
    all_fits <- dplyr::data_frame(
      tern        = character(),
      estimate    = numeric(),
      std.error   = numeric(),
      model.name  = character()
    )

    #iterate through each model's tidy dataframe of coefficients and append to a big labeled dataframe.
    for (i in 1:length(model_tables) ){
      all_fits <- rbind(
        all_fits,
        model_tables[[i]] %>%
          dplyr::mutate(model.name = model_names[i])
      )
    }
  } else {
    all_fits <- model_or_models %>% broom::tidy() %>%
      dplyr::mutate(model.name = "our model")
  }


  #how many models does a coefficient need to have to be plotted.
  #0 if we are plotting all, all models if we're only plotting common covs
  needed_model_num <- ifelse(only_in_common & multiple_models, length(model_or_models), 0)

  #sort dataframe so coefficients estimated for all models are at the top.
  all_fits_sorted <- all_fits %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(number_in_coeff = n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(number_in_coeff) %>%
    dplyr::filter(number_in_coeff >= needed_model_num) %>%
    dplyr::mutate(term = forcats::fct_inorder(term))


  #if the user has a param interpretation desired make it here. Useful to be able to disable as it doesn't fit all models
  if(model_interp == "default"){
    our_subtitle <- "Coefficient estimates represent the change in the expected outcome caused by changing\nthe given predictor's value one unit, while holding all other predictors constant.\n\nThe Confidence intervals represent the region of values for each coefficient that,\ngiven we were to collect data again from the same population and repeat the regression,\nthe interval will contain the true coefficient value 95% of the time."
  } else {
    our_subtitle = model_interp
  }


  #plot it
  plot_base <- ggplot2::ggplot(all_fits_sorted, ggplot2::aes(x = model.name, y = estimate)) +
    ggplot2::theme( axis.text.y = element_text(angle = -30, hjust = 1) )


  if (zero_line){
    plot_base <- plot_base + ggplot2::geom_hline(yintercept = 0, alpha = 0.5, color = "steelblue")
  }
  #if we only have a single model, putting the model name on the side is not exactly needed.
  if (!multiple_models){
    plot_base <- plot_base +
      ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks.y=ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank() ,
                     panel.grid.major.x = ggplot2::element_line( size=.1 )
      )
  }

  plot_base +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = estimate - 1.96*std.error,
                                          ymax = estimate + 1.96*std.error)) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%3.3f (%3.3f)", estimate, std.error)),
                       vjust = -0.8, size = 3)+
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~term, scales = "free_x") +
    ggplot2::labs(
      x = "", y = "Point Estimate and 95% CI",
      title = plot_title, subtitle = our_subtitle) +
    ggplot2::scale_y_continuous(expand = c(.15, .15))
}
