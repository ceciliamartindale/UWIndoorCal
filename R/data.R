#' Uncleaned PurpleAir data for examples
#'
#' A subset of data from Martindale et al 2025 to be used in examples
#'
#' @format ## `pa_unclean`
#' A data frame with 1,768 rows and 26 columns:
#' \describe{
#'   \item{mac_address}{Machine address}
#'   \item{current_dewpoint_f, current_temp_f}{Some of the PurpleAir channels}
#'   ...
#' }
#' @source Martindale et al 2025 (DOI to come)
"pa_unclean"

#' Cleaned PurpleAir data
#'
#' A subset of data from Martindale et al 2025 to be used in examples
#'
#' @format ## `pa_data`
#' A data frame with 1,768 rows and 26 columns:
#' \describe{
#'   \item{mac_address}{Machine address}
#'   \item{datetimehr}{Date-time by hour}
#'   \item{current_dewpoint_f, current_temp_f}{Some of the PurpleAir channels}
#'   ...
#' }
#' @source Martindale et al 2025 (DOI to come)
"pa_data"

#' Cleaned reference data
#'
#' A subset of data from Martindale et al 2025 to be used in examples
#'
#' @format ## `reg_data`
#' A data frame with 5,214 rows and 5 columns:
#' \describe{
#'   \item{datetime}{Hourly date-time}
#'   \item{ref_PM25}{Reference PM2.5 (ug/m^3)}
#'   ...
#' }
#' @source Martindale et al 2025 (DOI to come)
"reg_data"

#' Merged PurpleAir and reference data for calibration examples
#'
#' A subset of data from Martindale et al 2025 to be used in examples
#'
#' @format ## `cal_data`
#' A data frame with 1,768 rows and 24 columns:
#' \describe{
#'   \item{datetime}{Hourly date-time}
#'   \item{ref_PM25}{Reference PM2.5 (ug/m^3)}
#'   \item{p_0_3_um_ave, p_0_5_um_ave}{Particle count numbers less than specific diameters}
#'   ...
#' }
#' @source Martindale et al 2025 (DOI to come)
"cal_data"

#' Partitioned testing and training data for examples
#'
#' A subset of data from Martindale et al 2025 to be used in examples.
#'
#' @format ## `train_test`
#' A list of 2:
#' \describe{
#'   \item{train}{tibble with 1,176 obs. of 26 var}
#'   \item{test}{df with 553 obs. of 24 var}
#'   ...
#' }
#' @source Martindale et al 2025 (DOI to come)
"train_test"

#' Selected model example
#'
#' A subset of data from Martindale et al 2025 to be used in examples.
#' This is not the selected model from the paper -- it is only to be used in examples.
#'
#' @format ## `select_model`
#' A list of 25:
#' \describe{
#'   \item{method}{"lm"}
#'   \item{modelInfo}{Information about the model}
#'   \item{modelType}{Type of model}
#'   ...
#' }
#' @source Martindale et al 2025 (DOI to come)
"select_model"
