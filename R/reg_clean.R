#' Read in regulatory data, filter low values, set datetime interval
#'
#' @param path path to regulatory csv files. It is probably worth reading in
#'    one csv file using read_csv to get an idea of the structure and time formatting
#'    before loading in a list.
#' @param timezoneval the timezone local to where the regulatory data was collected.
#'     To get the list of all timezone names, use OlsonNames().
#' @param timeformat time format of the regulatory data (see ?strptime() for options).
#' @param time_var string name of time variable in regulatory csv file.
#' @param PM25_var string name of PM2.5 variable in regulatory csv file.
#' @param avgtime time interval to average measurements over. The default is 60 minutes.
#' @param low_threshold Lower threshold to filter out. Default is 2 ug/m^3 as this
#'     is the limit of the detection for many nephelometers.
#'
#' @returns a dataframe of regulatory data ready for use in calibration.
#' @export
#'
#' @examples
#' reg_data <- read_reg("DATA/REG", "US/Pacific", "%m/%d/%Y %I:%M:%S %p",
#'   skip=8, time_var=`Observation Time`, PM25_var=`Tukwila Allentown - Pm25Neph - ug/m3 - 1Hr Avg`)
read_reg <- function(path, timezoneval, timeformat, time_var=NULL,
                     PM25_var=NULL, avgtime=NULL, low_threshold=NULL) {

  reg.files <- list.files(path, recursive=T, full.names = T)
  data <- lapply(reg.files, data.table::fread) %>% data.table::rbindlist()

  if(!is_null(time_var)) {
    data <- data %>% dplyr::rename(datetime=time_var)
  }

  if(!is_null(PM25_var)) {
    data <- data %>% dplyr::rename(NPH_PM25=PM25_var)
  }

  if(is_null(low_threshold)) (low_threshold=2)
  data <- data %>% dplyr::mutate(datetime=as.POSIXct(datetime, tz=timezoneval,
                                              format=timeformat)) %>%
    dplyr::mutate(datetime=as.POSIXct(format(datetime, tz=timezoneval))) %>%
    dplyr::filter(NPH_PM25 > low_threshold)

  if(is_null(avgtime)) {avgtime="60 min"}
  data <- data[,datetimehr := lubridate::ceiling_date(datetime, avgtime)]

}

## Above: add an optional column for station name, add UTCdatetime column in the df
## change regulatory timezone variable to make it more clear what we need, give example of the Etc/GMT+8

#' Merge regulatory and PurpleAir data
#'
#' @param pa_data dataframe with cleaned PA data.
#' @param reg_data dataframe with cleaned regulatory data.
#' @param add_season binary 0 or 1, if 1 will create a season variable based on
#'    the time.
#' @param time_var name of time variable, if different from the default of datetimehr.
#'
#' @returns a dataframe of merged regulatory and PurpleAir data ready for calibration.
#' @export
#'
#' @examples
#' cal_data <- merge_reg_pa(pa_data, reg_data, add_season=1)
merge_reg_pa <- function(pa_data, reg_data, add_season, time_var=NULL) {
  if(!is_null(time_var)) {
    data <- data %>% dplyr::rename(datetimehr=time_var)
  }

  data.table::setkey(pa_data, datetimehr)
  data.table::setkey(reg_data, datetimehr)

  data <- reg_data[pa_data]

  if(add_season==1) {
    data <- data %>% dplyr::mutate(season=data.table::quarter(datetimehr))}
}
