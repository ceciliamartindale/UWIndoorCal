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

#' Use Lance Wallace's ALT method to correct PurpleAir measurements (with a
#' default calibration factor of 3).
#'
#' @param data cleaned PurpleAir data with averaged count channels
#' @param cf calibration factor. This defaults to 3 (see https://www.mdpi.com/1424-8220/22/7/2755).
#'
#' @returns PM2.5 measurements corrected with Lance Wallace's ALT method
#' (https://doi.org/10.1016/j.atmosenv.2021.118432)
#' @export
#'
#' @examples This assumes that the data has already been inspected.
#' cleaned_pa <- prep_pa_data(pa_df)
#' ALT_PA_data <- correct_ALT_method(cleaned_pa)
calibrate_ALT_method <- function(data, cf) {
   if (is.null(cf)) cf <- 3

   data %>% mutate(N_0.3_0.5 = (p_0_3_um_ave-p_0_5_um_ave)*1E4, # particles per deciliter
                N_0.5_1.0 = (p_0_5_um_ave-p_1_0_um_ave)*1E4,
                N_1.0_2.5 = (p_1_0_um_ave-p_2_5_um_ave)*1E4,
                V_0.3_0.5 = N_0.3_0.5*3.14*((0.387*1E-6)^3)/6, # diameter in microns, so the volume ends up being in microns^3/dL
                V_0.5_1.0 = N_0.5_1.0*3.14*((0.707*1E-6)^3)/6,
                V_1.0_2.5 = N_1.0_2.5*3.14*((1.581*1E-6)^3)/6,
                # need to convert from deciliter to cubic meters
                M_0.3_0.5 = V_0.3_0.5*1E12, # density units are kg/m^3, convert to ug/m^3
                M_0.5_1.0 = V_0.5_1.0*1E12,
                M_1.0_2.5 = V_1.0_2.5*1E12,
                ALT_PM2.5=cf*(M_0.3_0.5+M_0.5_1.0+M_1.0_2.5)) %>%
    select(-c(N_0.3_0.5, N_0.5_1.0, N_1.0_2.5, V_0.3_0.5, V_0.5_1.0, V_1.0_2.5,
              M_0.3_0.5, M_0.5_1.0, M_1.0_2.5))
}
