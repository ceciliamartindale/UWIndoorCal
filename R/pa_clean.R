#' Read in PurpleAir files
#'
#' @param path the path to the PurpleAir data (folder of csv files).
#' @param timezoneval the timezone local to where the PurpleAir data was collected.
#'     To get the list of all timezone names, use OlsonNames().
#'
#' @returns dataframe of PurpleAir observations
#' @export
#'
#' @examples
#' path <- system.file("extdata/PA_DATA", package = "UWIndoorCal")
#' pa_data <- read_pa(path, "US/Pacific")
read_pa <- function(path, timezoneval) {
  # column names needed
  pa_cols <- c("current_temp_f","current_humidity","current_dewpoint_f",
               "pressure", "adc", "mem", "rssi", "uptime", "pm1_0_cf_1",
               "pm2_5_cf_1","pm10_0_cf_1","pm1_0_atm", "pm2_5_atm","pm10_0_atm",
               "pm2.5_aqi_cf_1", "pm2.5_aqi_atm", "p_0_3_um", "p_0_5_um",
               "p_1_0_um", "p_2_5_um","p_5_0_um", "p_10_0_um","pm1_0_cf_1_b",
               "pm2_5_cf_1_b", "pm10_0_cf_1_b", "pm1_0_atm_b" ,"pm2_5_atm_b",
               "pm10_0_atm_b","pm2.5_aqi_cf_1_b","pm2.5_aqi_atm_b","p_0_3_um_b",
               "p_0_5_um_b","p_1_0_um_b","p_2_5_um_b","p_5_0_um_b","p_10_0_um_b",
               "gas")

  pa.files <- list.files(path, recursive=T, full.names = T)
  pa <- lapply(pa.files, data.table::fread) %>% data.table::rbindlist()

  # warning message for not having all the columns
  ifelse(pa_cols %in% colnames(pa) %>% unique() == TRUE, print("All variables present."),
         print("Missing necessary columns - check source data."))

  pa <- pa %>% dplyr::mutate(current_temp_f = as.numeric(current_temp_f),
           current_humidity = as.numeric(current_humidity),
           current_dewpoint_f = as.numeric(current_dewpoint_f),
           pressure = as.numeric(pressure), adc = as.numeric(adc),
           pm2_5_cf_1=as.numeric(pm2_5_cf_1),
           pm10_0_cf_1=as.numeric(pm10_0_cf_1),
           pm2_5_atm=as.numeric(pm2_5_atm),
           pm10_0_atm=as.numeric(pm10_0_atm),
           pm2.5_aqi_cf_1=as.numeric(pm2.5_aqi_cf_1),
           pm2.5_aqi_atm=as.numeric(pm2.5_aqi_atm),
           pm2_5_atm_b = as.numeric(pm2_5_atm_b),
           pm1_0_cf_1 = as.numeric(pm1_0_cf_1), pm1_0_atm = as.numeric(pm1_0_atm),
           p_0_3_um = as.numeric(p_0_3_um), p_0_5_um = as.numeric(p_0_5_um),
           p_1_0_um = as.numeric(p_1_0_um), p_2_5_um = as.numeric(p_2_5_um),
           p_5_0_um = as.numeric(p_5_0_um), p_10_0_um = as.numeric(p_10_0_um),
           pm1_0_cf_1_b = as.numeric(pm1_0_cf_1_b),
           pm2_5_cf_1_b = as.numeric(pm2_5_cf_1_b),
           pm10_0_cf_1_b = as.numeric(pm10_0_cf_1_b),
           pm1_0_atm_b = as.numeric(pm1_0_atm_b),
           pm10_0_atm_b = as.numeric(pm10_0_atm_b),
           pm2.5_aqi_cf_1_b = as.numeric(pm2.5_aqi_cf_1_b),
           pm2.5_aqi_atm_b = as.numeric(pm2.5_aqi_atm_b),
           p_0_3_um_b = as.numeric(p_0_3_um_b),
           p_0_5_um_b = as.numeric(p_0_5_um_b),
           p_1_0_um_b = as.numeric(p_1_0_um_b),
           p_2_5_um_b = as.numeric(p_2_5_um_b),
           p_5_0_um_b = as.numeric(p_5_0_um_b),
           p_10_0_um_b = as.numeric(p_10_0_um_b),
           gas=as.numeric(gas),
           datetimeUTC = as.POSIXct(UTCDateTime,  tz = "GMT",
                                    format="%Y/%m/%dT%H:%M:%Sz"),
           datetime = as.POSIXct(format(datetimeUTC,  tz = timezoneval)))

  pa
}

#' Get correlation of a and b channels by machine address
#'
#' @param data a dataframe of PurpleAir data, including the mac_address column
#' @param channela channel A variable, default is pm2_5_cf_1.
#' @param channelb channel B variable, default is pm2_5_cf_1_b.
#' @param threshold a PM2.5 value to assess correlation under. Default is 100 ug/m^3.
#' @param datetime datetime variable, default is datetime.
#'
#' @returns a printed dataframe of correlation between channels, maximum PM2.5,
#'     median PM2.5, mean PM2.5, number of observations, and machine address
#'     (mac_address).
#' @export
#'
#' @examples
#' pa_corr_ab(pa_unclean, channela="pm2_5_atm", channelb="pm2_5_atm_b", threshold=500)
#' pa_corr_ab(pa_unclean, threshold=150, datetime="UTCDateTime")
pa_corr_ab <- function(data, channela=NULL, channelb=NULL, threshold=NULL, datetime=NULL) {
  if (is.null(channela)) {
    channela <- rlang::sym("pm2_5_cf_1")
  } else {channela <- rlang::ensym(channela) }
  if (is.null(channelb)) {
    channelb <- rlang::sym("pm2_5_cf_1_b")
  } else {channelb <- rlang::ensym(channelb)}

  if (is.null(threshold)) (threshold=100)
  if (is.null(datetime)) (datetime="datetime")
  #print(channela)
  #print(channelb)

  if (length(unique(data$mac_address))==1) {
    corrvals <- data %>% dplyr::filter(!is.na(!!channela) & !is.na(!!channelb)) %>%
      dplyr::filter(!!channela < threshold & !!channelb < threshold) %>%
      dplyr::summarise(Correlation=stats::cor(!!channela,!!channelb),
                       max_a = max(!!channela, na.rm=TRUE),
                       max_b = max(!!channelb, na.rm=TRUE),
                       median_a = median(!!channela, na.rm=TRUE),
                       median_b = median(!!channelb, na.rm=TRUE),
                       mean_a = mean(!!channela, na.rm=TRUE),
                       mean_b = mean(!!channelb, na.rm=TRUE), obs = length(datetime))
  }
  if (length(unique(data$mac_address)) > 1) {
    corrvals <- data %>% dplyr::filter(!is.na(!!channela) & !is.na(!!channelb)) %>%
      dplyr::filter(!!channela < threshold & !!channelb < threshold) %>%
      dplyr::group_by(mac_address) %>%
      dplyr::summarise(Correlation=stats::cor(!!channela,!!channelb),
                       max_a = max(!!channela, na.rm=TRUE),
                     max_b = max(!!channelb, na.rm=TRUE),
                     median_a = median(!!channela, na.rm=TRUE),
                     median_b = median(!!channelb, na.rm=TRUE),
                     mean_a = mean(!!channela, na.rm=TRUE),
                     mean_b = mean(!!channelb, na.rm=TRUE), obs = length(datetime))
  }

  print(corrvals)
}

#' Plot correlation of A and B channels
#'
#' @param data dataframe containing PurpleAir data.
#' @param channela channel A variable, default is pm2_5_cf_1.
#' @param channelb channel B variable, default is pm2_5_cf_1_b.
#' @param threshold a PM2.5 value to assess correlation under. Default is 500 ug/m^3.
#'
#' @returns plot of correlation of A and B channels.
#' @export
#'
#' @examples
#' pa_corr_plot(pa_unclean)
#'
#' pa_corr_plot(pa_unclean, "pm2_5_atm", "pm2_5_atm_b")
pa_corr_plot <- function(data, channela=NULL, channelb=NULL, threshold=NULL) {

  if (is.null(channela)) {
    channela <- rlang::sym("pm2_5_cf_1")
    } else {channela <- rlang::ensym(channela) }
  if (is.null(channelb)) {
    channelb <- rlang::sym("pm2_5_cf_1_b")
  } else {channelb <- rlang::ensym(channelb)}

  if (is_null(threshold)) {
    data <- data %>% dplyr::filter(!!channela < 500 & !!channelb < 500)
  } else (data <- data %>% dplyr::filter(!!channela < threshold & !!channelb < threshold))

  labx <- data %>% dplyr::select(channela) %>% max(na.rm=T)/1.5
  laby <- data %>% dplyr::select(channela) %>% max(na.rm=T)/4

  ggplot2::ggplot(data, ggplot2::aes(x=!!channela, y=!!channelb))+
  ggplot2::geom_point(pch=1)+ ggplot2:: ggtitle("PM2.5 Channel Agreement") +
  ggplot2::geom_smooth(method="lm", color="black") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5, color="grey32")) +
  ggplot2::xlab(as.character(channela)) + ggplot2::ylab(as.character(channelb)) +
  ggpubr::stat_cor(ggplot2::aes(label=ggplot2::after_stat(rr.label)),
                   label.x=labx, label.y=laby) +
    ggplot2::facet_wrap(~"mac_address")
}

#' Plot time series of A and B channels
#'
#' @param data dataframe containing PurpleAir data
#' @param timescale time period string to show on the x-axis.
#' @param channela channel A variable, default is pm2_5_cf_1.
#' @param channelb channel B variable, default is pm2_5_cf_1_b.
#'
#' @returns timeseries plots of channel A and channel B on the same plot
#' @export
#'
#' @examples
#' pa_timeseries(pa_unclean, "1 month")
#' pa_timeseries(pa_unclean, "3 weeks")
pa_timeseries <- function(data, timescale, channela=NULL, channelb=NULL) {

  if (is.null(channela)) {
    channela <- rlang::sym("pm2_5_cf_1")
  } else {channela <- rlang::ensym(channela) }
  if (is.null(channelb)) {
    channelb <- rlang::sym("pm2_5_cf_1_b")
  } else {channelb <- rlang::ensym(channelb)}

  ggplot2::ggplot() +
    ggplot2::ggtitle("Time series inspection") +
    ggplot2::geom_point(data,
          mapping=ggplot2::aes(x=datetime, y=!!sym(channela), color = as.character(channela)),
          pch=".", size =1.5, alpha=0.5) +
    ggplot2::geom_point(data,
          mapping=ggplot2::aes(x=datetime, y=!!sym(channelb), color = as.character(channelb)),
               pch=".", size =1.5, alpha=0.5) +
    ggplot2::xlab("Observation Month") + ggplot2::ylab("PM2.5 Concentration (ug/m3)") +
    ggplot2::scale_x_datetime(date_breaks=timescale) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = 0.5)) +
    ggplot2::facet_wrap(~"mac_address")
}

#' Prepare PurpleAir data for calibration
#'
#' @param data PurpleAir data in a dataframe. This function assumes that the order
#'   of the PurpleAir columns has not been modified. Columns can have been added
#'   to the end, but the default PurpleAir export should not have changed.
#' @param channel Channel to filter data based on. The default is pm2_5_cf_ave,
#'   the averaged cf PM2.5 channel. We recommend using an averaged column such
#'   as pm2_5_atm_ave, pm2_5_cf_ave, or pm2.5_aqi_cf_ave.
#' @param low_threshold Lower threshold to filter out. Default is 2 ug/m^3.
#'   We recommend inspecting your data well to determine an appropriate threshold
#'   value for cleaning.
#' @param high_threshold Higher threshold to filter out. Default is 500 ug/m^3.
#'   Note that this should be carefully assessed, especially in periods of high smoke.
#'   We recommend inspecting your data well to determine
#'   an appropriate threshold value for cleaning.
#' @param avgtime time period to average measurements over. The default is 60 minutes,
#'  which provides hourly air quality measurements. We use the "ceiling time",
#'  which would mean that for an hourly average at 1PM, we average the measurements
#'  collected between 12:01PM and 1PM.
#' @param station name of station to calibrate off of, based on the reference
#' data you provide. This will create a variable with the reference station name,
#' which is important if calibrating different sensors using different reference
#' stations.
#'
#' @returns a dataframe of PurpleAir data ready for calibration, averaged by avgtime
#'  (default is one hour). Data should be thoroughly expected for agreement between
#'  a and b channels and obvious dysfunction before this function.
#' @export
#'
#' @examples
#' pa_data_cal <- prep_pa_data(pa_unclean)
#' pa_data_cal <- prep_pa_data(pa_unclean, channel="pm2.5_aqi_cf_ave", low_threshold=0,
#'  high_threshold=300)
prep_pa_data <- function(data, channel="pm2_5_cf_ave", low_threshold=NULL, high_threshold=NULL, avgtime=NULL,
                         station=NULL) {
  if(is_null(low_threshold)) (low_threshold=2)
  if(is_null(high_threshold)) (high_threshold=500)
  if(is_null(avgtime)) (avgtime="60 min")

  channelspa = colnames(data)[13:40]

  data <- data %>%
    dplyr::mutate(datetimehr=lubridate::ceiling_date(datetime, avgtime))
  data <- data[, lapply(.SD, mean, na.rm = TRUE),
                                  by = c("mac_address", "datetimehr"),
                                  .SDcols = c("current_dewpoint_f",
                                              "current_temp_f",
                                              "current_humidity",
                                              "pressure",
                                               channelspa)]

  #avg all the channels - edit this to be averaging
  data <- data %>% dplyr::mutate(pm2_5_cf_ave=(pm2_5_cf_1+pm2_5_cf_1_b)/2,
                                 pm10_0_cf_ave=(pm10_0_cf_1+pm10_0_cf_1_b)/2,
                                 pm2_5_atm_ave=(pm2_5_atm+pm2_5_atm_b)/2,
                                 pm10_0_atm=(pm10_0_atm+pm10_0_atm_b)/2,
                                 pm2.5_aqi_cf_ave=(pm2.5_aqi_cf_1+pm2.5_aqi_cf_1_b)/2,
                                 pm2.5_aqi_atm_ave=(pm2.5_aqi_atm+pm2.5_aqi_atm_b)/2,
                                 pm1_0_cf_ave = (pm1_0_cf_1+pm1_0_cf_1_b)/2,
                                 pm1_0_atm_ave = (pm1_0_atm+pm1_0_atm_b)/2,
                                 p_0_3_um_ave = (p_0_3_um+p_0_3_um_b)/2,
                                 p_0_5_um_ave = (p_0_5_um+p_0_5_um_b)/2,
                                 p_1_0_um_ave = (p_1_0_um+p_1_0_um_b)/2,
                                 p_2_5_um_ave = (p_2_5_um+p_2_5_um_b)/2,
                                 p_5_0_um_ave = (p_5_0_um+p_5_0_um_b)/2,
                                 p_10_0_um_ave = (p_10_0_um+p_10_0_um_b)/2) %>%
    dplyr::select(-c(pm10_0_cf_1, pm10_0_cf_1_b,
              pm10_0_atm, pm10_0_atm_b, pm2.5_aqi_atm,
              pm2.5_aqi_atm_b, pm1_0_cf_1,
              pm1_0_cf_1_b, pm1_0_atm, pm1_0_atm_b, p_0_3_um, p_0_3_um_b,
              p_0_5_um, p_0_5_um_b, p_1_0_um, p_1_0_um_b, p_2_5_um, p_2_5_um_b,
              p_5_0_um, p_5_0_um_b, p_10_0_um, p_10_0_um_b))

    if (is_null(channel)) {
      channel <- rlang::eval_tidy("pm2_5_cf_ave", data)
    } else (channel <- rlang::eval_tidy(channel, data))

    data <- data %>% dplyr::filter(!!sym(channel) > low_threshold) %>%
    dplyr::filter(!!sym(channel) < high_threshold) %>%
    dplyr::filter(!is.na(current_humidity) & !is.na(current_temp_f))

    if (!is_null(station)) {
      data <- data %>% mutate(station=station)
    }
}

## Could consider adding an argument to modify the variables that are selected out
# not urgent but could be a compromise of not deciding - add this list
