#' Calibrate PA data based on regression model
#'
#' @param data
#' @param model
#'
#' @returns
#' @export
#'
#' @examples
calibrate_PA_data <- function(data, model) {
  data %>% mutate(calibrated_pm25 = predict(model, newdata = data))
}
