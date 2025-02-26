#' Do cross-validation (author: Magali Blanco)
#'
#' @param data dataframe containing the training data.
#' @param id unique variable for determining sort order of data frame
#' @param group grouping variable (a variable in the data frame)
#' @param formula formula to pass to lm
#'
#' @returns dataset with a new variable called cvpreds appended to the end;
#'    these are the out-of-sample predictions
#' @export
#'
#' @examples
#' formula <- NPH_PM25 ~ 0 + p_0_3_um_ave + p_0_5_um_ave + p_1_0_um_ave +
#'     p_2_5_um_ave + current_temp_f
#' do_CV(train_data, id = "train_index", group = "select_index", formula)
do_CV <- function(data, id = "id", group = "group", formula) {

  # do for each cluster in the  dataset
  # (Note the use of "[[ ]]" rather than "$" because group is input in the
  # function call as a quoted variable)
  lapply(unique(data[[group]]), function(this_group){

    # fit the "common" model to the training set (without this group)
    CV_lm <- lm(formula, data = data[data[[group]] != this_group,])

    # generate predictions for this group using training model
    data[data[[group]] == this_group,] %>%
      mutate(cvpreds = predict(CV_lm, newdata = .),
             AIC = AIC(CV_lm))

    # recombine data from all clusters and sort by ID column
    # note use of ".data[[ ]]" to return the value of variable id
  }) %>% bind_rows() %>% arrange(.data[[id]])

  # return the dataset (the last-evaluated object is always returned by default)
}

#' Do cross-validation with multiple formulas.
#'
#' @param data dataframe containing the training data.
#' @param id unique variable for determining sort order of data frame
#' @param group grouping variable (a variable in the data frame)
#' @param formulas list of formulas to pass to lm
#'
#' @returns dataframe with CV_preds and AIC columns for each variable
#' @export
#'
#' @examples
#' formulas <- list(formula1 = NPH_PM25 ~ 0 + pm2_5_cf_ave +
#'    current_humidity_outdoor + I(current_humidity^2) +
#'    current_temp_f + as.factor(season),
#'                  formula2 = NPH_PM25 ~ 0 + pm2_5_cf_ave +
#'    current_humidity + current_temp_f + as.factor(season),
#'                  formula3 = NPH_PM25 ~ 0 + pm2_5_cf_ave +
#'    current_temp_f + as.factor(season),
#'                  formula4 = NPH_PM25 ~ 0 + pm2_5_cf_ave + as.factor(season))
#' cv_data <- do_CV_mult(train_data, "train_index", "select_index", formulas)
do_CV_mult <- function(data, id = "id", group = "group", formulas) {
  purrr::reduce(seq_along(formulas), function(data, model_num) {
    data %>%
      do_CV(id, group, formulas[model_num]) %>%
      rename_with(~ c(paste0("cvpreds_model_", model_num), paste0("AIC_model_", model_num)),
                  .cols = c(cvpreds, AIC))
  }, .init = data)
}


# get_MSE

#' Get mean-squared error (MSE) (Author: Magali Blanco)
#'
#' @param obs outcome variable
#' @param pred prediction variable from a model
#' @param AIC_var AIC variable
#'
#' @returns dataframe of RMSE, MSE-based R^2, and AIC.
#' @export
#'
#' @examples
#' one_MSE <- get_MSE(train_data_CV$NPH_PM25, train_data_CV$cvpreds_model1, train_data_CV$AIC_model1)
#' rbind(get_MSE(train_data_CV$NPH_PM25, train_data_CV$cvpreds_model_1, train_data_CV$AIC_model_1),
#'   get_MSE(train_data_CV$NPH_PM25, train_data_CV$cvpreds_model_2, train_data_CV$AIC_model_2),
#'   get_MSE(train_data_CV$NPH_PM25, train_data_CV$cvpreds_model_3, train_data_CV$AIC_model_3),
#'   get_MSE(train_data_CV$NPH_PM25, train_data_CV$cvpreds_model_4, train_data_CV$AIC_model_4))
get_MSE <- function(obs,pred, AIC_var) {
  # obs is the outcome variable
  # pred is the prediction from a model

  # mean of obs
  obs_avg <- mean(obs)

  # MSE of obs (for R2 denominator)
  MSE_obs <- mean((obs-obs_avg)^2)

  # MSE of predictions
  MSE_pred <- mean((obs - pred)^2)

  # compile output
  result <- c(RMSE = sqrt(MSE_pred),
              MSE_based_R2 = max(1 - MSE_pred / MSE_obs, 0),
              AIC = mean(AIC_var))

  # explicit return (optional)
  return(result)
}

#' Plot the correlation of the observed PM2.5 versus the predicted calibration.
#'
#' @param cv_data dataframe containing the cross-validated predictions.
#' @param obs name of variable containing the observed PM2.5
#' @param pred name of variable containing the predicted PM2.5
#' @param model_name name of model for title
#'
#' @returns plot observed vs. predicted PM2.5
#' @export
#'
#' @examples
#' plot_CV_corr(cv_merged, NPH_PM25, cvpreds_model_3, "Model 3")
plot_CV_corr <- function(cv_data, obs="obs", pred="pred", model_name="name") {
  # get range for plot
  r <- cv_data %>% dplyr::select(obs, pred) %>% range()
  print(r)

  # plot data
  ggplot2::ggplot(data = cv_data, ggplot2::aes(!!sym(obs), !!sym(pred))) +
    ggplot2::geom_point(shape = "o", alpha = 0.8) +
    ggplot2::lims(x= r, y = r) +
    ggplot2::coord_fixed() +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "blue") +
    ggplot2::labs(title = paste0("Time-sliced CV for ", model_name),
         x = "EPA NPH",
         y = "Predicted PM2.5") +
    ggplot2::theme_bw()
}
