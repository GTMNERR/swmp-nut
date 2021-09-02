# ----01 info----
# must run 01_load_wrangle.R first
# use data frame dat3 for analysis and graphics

# ----02 figure function all stations----
# run code, then use function.
allstation_plot <- function(param, axis_y_title) {
  
  dat3 %>%
    dplyr::filter(monitoringprogram == 1) %>%
    dplyr::mutate(date = lubridate::date(date_sampled)) %>%
    dplyr::select(station_code, date, cdmo_name, result) %>%
    dplyr::group_by(station_code, date, cdmo_name) %>%
    dplyr::summarise(result2 = mean(result, na.rm = TRUE)) %>%
    dplyr::filter(cdmo_name == param) %>%
    ggplot(aes(x = date, y = result2, color = station_code)) +
    geom_point(size = 3) +
    geom_line(size = 1, linetype = "dashed") +
    scale_x_date(limits = c(as.Date("2019-01-01"), as.Date("2019-12-31")),
      date_breaks = "month", date_labels = "%b") +
    theme_cowplot() +
    labs(y = axis_y_title,
         title = param)
}

# ----03 figure function for thresholds----
chla_threshold_plot <- function(station, threshold_low) {
  
  dat3 %>%
    dplyr::filter(monitoringprogram == 1) %>%
    dplyr::mutate(date = lubridate::date(date_sampled)) %>%
    dplyr::select(station_code, date, cdmo_name, result) %>%
    dplyr::group_by(station_code, date, cdmo_name) %>%
    dplyr::summarise(result2 = mean(result, na.rm = TRUE)) %>%
    dplyr::filter(cdmo_name == 'CHLA_N' & station_code == station) %>%
    ggplot(aes(x = date, y = result2)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_hline(yintercept = threshold_low, linetype = "dashed", 
               size = 2, color = "lightblue") +
    geom_hline(yintercept = 20, linetype = "dashed", 
               size = 2, color = "orange") +
    theme_cowplot() +
    labs(y = chla_y_title,
         title = station,  
         subtitle = paste('low threshold =', threshold_low))
}
  
# ----04 figure function pellicer ISCO----
# run code, then use function.
isco_graph <- function(param, monthselect, axis_y_title) {
  
  # filters out ISCO data
  # creates months as numbers
  # selects only relevant parameters
  # opens view to see data
  View(dat3 %>%
         dplyr::filter(monitoringprogram == 2) %>%
         dplyr::mutate(month = lubridate::month(date_sampled)) %>%
         dplyr::select(month, date_sampled, cdmo_name, result) %>%
         dplyr::filter(month == monthselect & cdmo_name == param) %>%
         tidyr::pivot_wider(id_cols = c('date_sampled'), 
                            names_from = cdmo_name,
                            values_from = result)
  )
  
  # filters out ISCO data
  # creates months as numbers
  # selects only relevant parameters
  dat3 %>%
    dplyr::filter(monitoringprogram == 2) %>%
    dplyr::mutate(month = lubridate::month(date_sampled)) %>%
    dplyr::select(month, date_sampled, cdmo_name, result) %>%
    dplyr::filter(month == monthselect & cdmo_name == param) %>%
    ggplot(aes(x = date_sampled, y = result)) +
    geom_point(size = 3) +
    geom_line(size = 1, linetype = "dashed") +
    scale_x_datetime(date_breaks = "4 hour", date_labels = "%b %d %I:%M") +
    theme_cowplot() +
    labs(y = axis_y_title,
         title = param)
  
}