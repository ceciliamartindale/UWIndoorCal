#' Calibrate PA data based on regression model
#'
#' @param data - to add description
#' @param model - to add description
#'
#' @returns calibrated PA data
#' @export
#'
#' @examples to add
calibrate_PA_data <- function(data, model) {
  data %>% mutate(calibrated_pm25 = predict(model, newdata = data))
}
