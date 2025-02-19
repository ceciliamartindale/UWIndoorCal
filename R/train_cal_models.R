#' Partition data for calibration using time-sliced approach
#'
#' @param data air quality data for calibration. Should include channels of
#'    interest for calibration and regulatory reference values for matching time
#'    period.
#' @param proportion proportion of data to be used in the training dataset (example: 0.7).
#' @param initial_Window initial number of observations to be included in a timeslice.
#'    If using hourly data, 168 is one week of hourly observations.
#' @param seed_val value to set seed.
#'
#' @returns a list of two dataframes, one of training data and one of testing data.
#' @export
#'
#' @examples
#' train_test <- partition(cal_data, 0.7, 336, 5)
#' train_test_two_week_slice <- partition(cal_data, 0.7, 168, 5)
partition <- function(data, proportion, initial_Window, seed_val) {
  time_slices <- caret::createTimeSlices(1:nrow(data),
                                initialWindow = initial_Window,
                                #horizon = 0.1*round(nrow(pa_reg) * proportion),
                                skip = initial_Window-1,
                                fixedWindow = TRUE)

# Extract training and testing indices: get random variable between 1 and 20, select half of them.
  set.seed(seed_val)
  select_indices = sample(1:length(time_slices$train),
                        replace = F) [1:(length(time_slices$train)*proportion)]
  train_indices <- unlist(lapply(select_indices,
                               FUN = function(i)
                                 time_slices$train[[i]]))

  min_indices <- unlist(lapply(time_slices$train, min))[select_indices]
  max_indices <- unlist(lapply(time_slices$train, max))[select_indices]

# create training data
  train_data <- data[train_indices, ] %>%
    dplyr::mutate(train_index = train_indices) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(select_index = dplyr::coalesce(
      purrr::map2_dbl(min_indices, max_indices,
             ~ if_else(between(train_index, .x, .y),
                       select_indices[which(min_indices == .x)], NA_real_)) %>%
      discard(is.na) %>%
      dplyr::first(),
      NA_real_
    )) %>%
    dplyr::ungroup()

# create testing data
  test_data <- data[-train_indices, ] #%>% dplyr::filter(NPH_PM25 >= 2)

  train_and_test_data <- list(train= train_data, test=test_data)
  train_and_test_data
}

# train models
# get results of trained models
