#' Read in reference data, filter low values, set datetime interval
#'
#' @param path path to reference csv files. It is probably worth reading in
#'    one individual csv file using read_csv to get an idea of the structure and
#'    time formatting before loading in a list. Data for one reference station
#'    should be in a folder with that station name. Sub-folders will also be loaded.
#' @param timezoneval the timezone local to where the reference data was collected.
#'     To get the list of all timezone names, use OlsonNames().
#' @param timezone_etc the timezone local to where the reference data was collected
#'     in "Etc/GMT+8" format (the format that does not observe daylight savings time),
#'     as regulatory monitors generally do not observe daylight savings time.
#' @param timeformat time format of the reference data (see ?strptime() for options).
#' @param skip number of header rows of original file to skip (see fread
#' documentation for more). Be sure to retain column names.
#' @param time_ind numeric column index or column name of time variable in reference csv file.
#' @param PM25_ind column index of PM2.5 variable in reference csv file.
#' @param avgtime time interval to average measurements over. The default is 60 minutes.
#'  We use the "ceiling time",
#'  which would mean that for an hourly average at 1PM, we average the measurements
#'  collected between 12:01PM and 1PM.
#' @param low_threshold Lower threshold to filter out. Default is 2 ug/m^3. We
#'  recommend inspecting your data and equipment limit of detection well to
#'  determine an appropriate threshold value for cleaning.
#'
#' @returns a dataframe of reference measurements ready for use in calibration with
#'  station names, UTC time, and local time. The default renames PM2.5 measurements
#'  to "ref_PM25".
#' @export
#'
#' @examples
#' path <- system.file("extdata/REG", package = "lcscal")
#' reg_data <- read_reg(path, "US/Pacific", "Etc/GMT+8", "%m/%d/%Y %I:%M:%S %p",
#'   time_ind=1, PM25_ind=2,skip=8)
read_reg <- function(path, timezoneval, timezone_etc, timeformat, time_ind,
                     PM25_ind, skip=NULL, avgtime=NULL, low_threshold=NULL) {

  if(is_null(skip)) {
    skip <- 0
  }

  reg.files <- list.files(path, recursive=T, full.names = T)
  data <- lapply(reg.files, data.table::fread, skip = skip)
  data <- lapply(1:length(data), FUN=function(i){
    data[[i]] %>% dplyr::rename("ref_PM25" = PM25_ind,
                                      "datetime" = time_ind) %>%
      dplyr::mutate(station = basename(dirname(reg.files[[i]])))
      # colnames(data[[i]])[time_ind] <- "datetime"
      # colnames(data[[i]])[PM25_ind] <- "ref_PM25"
      }) %>%
    data.table::rbindlist()

  if(is_null(low_threshold)) (low_threshold=2)
  data <- data %>% dplyr::mutate(datetime=as.POSIXct(datetime, tz=timezoneval,
                                              format=timeformat)) %>%
    dplyr::mutate(datetime=as.POSIXct(datetime, tz=timezone_etc, format="%m/%d/%Y %I:%M:%S %p")) %>%
    dplyr::mutate(datetime=as.POSIXct(format(datetime, tz=timezoneval)),
                  datetimeUTC=as.POSIXct(datetime, tz="UTC")) %>%
    dplyr::filter(ref_PM25 > low_threshold)

  if(is_null(avgtime)) {avgtime="60 min"}
  data <- data %>%
    dplyr::mutate(datetimehr = lubridate::ceiling_date(datetime, avgtime))

}

#' Merge reference and PurpleAir data
#'
#' @param data_1 dataframe with cleaned PA data.
#' @param data_2 dataframe with cleaned reference data.
#' @param add_season binary 0 or 1, if 1 will create a season variable based on
#'    the calendar year. This will be an integer where 1 = winter, 2 = spring, 3 = summer,
#'    and 4 = fall. If data already has a season variable, set this to 0 so it is not
#'    overwritten.
#' @param time_var name of time variable, if different from the default of datetimehr.
#'
#' @returns a dataframe of merged reference and PurpleAir data ready for calibration.
#' @export
#'
#' @examples
#' cal_data <- merge_reg_pa(pa_data, reg_data, add_season=1)
merge_reg_pa <- function(data_1, data_2, add_season, time_var=NULL) {
  if(!is.null(time_var)) {
    data_1 <- data_1 %>% dplyr::rename(datetimehr=time_var)
  }

  data.table::setkey(data_1, datetimehr)
  data.table::setkey(data_2, datetimehr)

  data <- data_2[data_1, on = c("station", "datetimehr")]

  if(add_season==1) {
    data <- data %>% dplyr::mutate(season=data.table::quarter(datetimehr))}

  data <- data %>% dplyr::filter(!is.na(datetimehr))
}
