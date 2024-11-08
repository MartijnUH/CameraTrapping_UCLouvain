## Functions used in CT_case_study.R

#' Extract the set of coordinates for each deployment in a CTDP
#'
#' @usage get_coordinates(ctdp)
#'
#' @param ctdp A Camera Trap Data Package as imported by camtraptor::read_camtrapdp
#'
#' @return A Rx2 matrix, where R is the number of unique deployments and 
#' columns 1 and 2 contain the x- and y-coordinates respectively.
#' 
#' @author
#' Martijn Bollen

get_coordinates <- function(ctdp) {
  ctdp$data$deployments %>% 
    dplyr::group_by(locationName) %>%
    dplyr::summarize(longitude = unique(longitude), 
                     latitude = unique(latitude)) %>%
    dplyr::rename(Station = locationName)
}

#' Append radian clock and solar times to a record table
#'
#' @usage append_activity_data(ctdp)
#'
#' @param record_table A record table as computed by camtraptor::get_record_table
#' @param coordinates A Rx2 matrix containing unique x- and y-coordinates for R deployments
#'
#' @return A record table with 4 columns appended. The x- and y-coordinates,
#' radian clock and radian solar time of the observation.
#' 
#' @author
#' Martijn Bollen

append_activity_data <- function(record_table, coordinates) {
  
  # get species records
  out <- dplyr::filter(record_table, !is.na(Time))
  out <- dplyr::left_join(out, coordinates, by = "Station")
  cmat <- matrix(c(out$longitude, out$latitude), ncol = 2)
  
  # calculate clock time and sun time of each record
  out <- out %>% dplyr::mutate(
    clock = gettime(DateTimeOriginal), # clock time in radians
    solar = sunTime(clock, DateTimeOriginal, cmat) # sun time at c(lon, lat) in radians
  )
  
  return(out)
}

#' Plot clock and solar times using ggplot
#'
#' @usage actplot_ss(data)
#'
#' @param data A record table as computed by camtraptor::get_record_table and append_activity_data
#'
#' @return A ggplot 
#' 
#' @author
#' Martijn Bollen
#' 
actplot_ss <- function(data, xlabels, ...) {
  brks <- c(0, pi/2, pi, (3*pi)/2, 2*pi)
  lbls_solar <- c("midnight", "sunrise", "noon", "sunset", "midnight")
  lbls_clock <- c("00:00", "06:00", "12:00", "18:00", "00:00")
  lbls <- switch(xlabels, solar = lbls_solar, clock = lbls_clock)
  
  data %>%
    ggplot(aes(x = x, y = y, ymin = lcl, ymax = ucl)) +
    geom_line(col = ...) + 
    geom_ribbon(alpha = 0.2, col = NA, fill = ...) +
    geom_vline(xintercept = brks[c(2,4)], linetype = "dashed", col = "black") +
    labs(x = "", y = "Density") +
    scale_x_continuous(breaks = brks, labels = lbls) +
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5))
}

actplot_ms <- function(data, xlabels, ...) {
  brks <- c(0, pi/2, pi, (3*pi)/2, 2*pi)
  lbls_solar <- c("midnight", "sunrise", "noon", "sunset", "midnight")
  lbls_clock <- c("00:00", "06:00", "12:00", "18:00", "00:00")
  lbls <- switch(xlabels, solar = lbls_solar, clock = lbls_clock)
  
  data %>%
    ggplot(aes(x = x, y = y, ymin = lcl, ymax = ucl, col = species, fill = species)) +
    geom_line() + 
    geom_ribbon(alpha = 0.2, col = NA) +
    geom_vline(xintercept = brks[c(2,4)], linetype = "dashed", col = "black") +
    labs(x = "", y = "Density", col = "Species:", fill = "Species:") +
    scale_x_continuous(breaks = brks, labels = lbls) +
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
}
