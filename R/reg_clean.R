#' Read in regulatory data, filter low values, set datetime interval
#'
#' @param path path to regulatory csv files. It is probably worth reading in
#'    one csv file using read_csv to get an idea of the structure and time formatting
#'    before loading in a list.
#' @param timezoneval the timezone local to where the regulatory data was collected.
#'     To get the list of all timezone names, use OlsonNames().
#' @param timezone_etc the timezone local to where the regulatory data was collected
#'     in "Etc/GMT+8" format (the format that does not observe daylight savings time),
#'     as regulatory monitors generally do not observe daylight savings time.
#' @param timeformat time format of the regulatory data (see ?strptime() for options).
#' @param skip number of rows of original file to skip (see fread documentation for more).
#' @param time_ind column index of time variable in regulatory csv file.
#' @param PM25_ind column index of PM2.5 variable in regulatory csv file.
#' @param avgtime time interval to average measurements over. The default is 60 minutes.
#' @param low_threshold Lower threshold to filter out. Default is 2 ug/m^3 as this
#'     is the limit of the detection for many nephelometers.
#'
#' @returns a dataframe of regulatory data ready for use in calibration with station
#'    names, UTC time, and local time.
#' @export
#'
#' @examples
#' reg_data <- read_reg("DATA/REG", "US/Pacific", "%m/%d/%Y %I:%M:%S %p",
#'   time_ind=1, PM25_ind=2,skip=8)
read_reg <- function(path, timezoneval, timezone_etc, timeformat, time_ind,
                     PM25_ind, skip=NULL, avgtime=NULL, low_threshold=NULL) {

  if(is_null(skip)) {
    skip <- 0
  }

  reg.files <- list.files(path, recursive=T, full.names = T)
  data <- lapply(reg.files, data.table::fread, skip = skip)
  data <- lapply(1:length(data), FUN=function(i){
      colnames(data[[i]])[time_ind] <- "datetime"
      colnames(data[[i]])[PM25_ind] <- "NPH_PM25"
      dplyr::mutate(data[[i]], station = basename(dirname(reg.files[[i]])))
      }) %>%
    data.table::rbindlist()

  if(is_null(low_threshold)) (low_threshold=2)
  data <- data %>% dplyr::mutate(datetime=as.POSIXct(datetime, tz=timezoneval,
                                              format=timeformat)) %>%
    dplyr::mutate(datetime=as.POSIXct(datetime, tz=timezone_etc, format="%m/%d/%Y %I:%M:%S %p")) %>%
    dplyr::mutate(datetime=as.POSIXct(format(datetime, tz=timezoneval)),
                  datetimeUTC=as.POSIXct(datetime, tz="UTC")) %>%
    dplyr::filter(NPH_PM25 > low_threshold)

  if(is_null(avgtime)) {avgtime="60 min"}
  data <- data[,datetimehr := lubridate::ceiling_date(datetime, avgtime)]

}

#' Merge regulatory and PurpleAir data
#'
#' @param data_1 dataframe with cleaned PA data.
#' @param data_2 dataframe with cleaned regulatory data.
#' @param add_season binary 0 or 1, if 1 will create a season variable based on
#'    the time. This will be an integer where 1 = winter, 2 = spring, 3 = summer,
#'    and 4 = fall.
#' @param time_var name of time variable, if different from the default of datetimehr.
#'
#' @returns a dataframe of merged regulatory and PurpleAir data ready for calibration.
#' @export
#'
#' @examples
#' cal_data <- merge_reg_pa(pa_data, reg_data, add_season=1)
merge_reg_pa <- function(data_1=pa_data, data_2=reg_data, add_season, time_var=NULL) {
  if(!is_null(time_var)) {
    data_1 <- data_1 %>% dplyr::rename(datetimehr=time_var)
  }

  data.table::setkey(data_1, datetimehr)
  data.table::setkey(data_2, datetimehr)

  data <- data_2[data_1, on = c("station", "datetimehr")]

  if(add_season==1) {
    data <- data %>% dplyr::mutate(season=data.table::quarter(datetimehr))}
}
