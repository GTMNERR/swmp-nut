# 01 Nutrients ---------------------------------------------------------------

## 01.1 monthly-boxplot-w-current-YR ----

boxplot_currentyear <- function(station, param, threshold) {
  # parameters: station, param, threshold
  # 0 is CHLA_avg, 1 is TN_avg, 2 is TP_avg
  
  if (param == 0) {  
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = chla_n)) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "",
           y = "Mean Monthly Chlorophyll-a (\U00B5g/L)")
  } else if (param == 1) {
    
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = tn)) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "",
           y = "Mean Monthly Total Nitrogen (mg/L)")
    
  } else {
    boxplot <- ggplot(data = NUT_monthly, 
                      aes(x = month_abb, y = tp)) +
      geom_boxplot(data = filter(NUT_monthly, station_code == station & year < 2022), 
                   aes(fill = "2002-2021")) +
      geom_point(data = filter(NUT_monthly, station_code == station & year == 2022), 
                 aes(color = "2022"),
                 size = 4) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", 
                         values = c("2022" = "red")) +
      scale_fill_manual(name = "",
                        values = c("2002-2021" = "white")) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.text = element_text(color = "black")) +
      labs(x = "",
           y = "Mean Monthly Total Phosphorus (mg/L)")
  }
}

# pc_chla_boxplot <- boxplot_currentyear(station = "gtmpcnut", param = 0, threshold = 4.3)
# plotly::ggplotly(pc_chla_boxplot + labs(x = "",
#                        y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
#                        title = "Pellicer Creek") +
#   annotate("text",
#            x = "Mar",
#            y = 38,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 4.3 (\U00B5g/L)")
# )

# boxplot_currentyear(station = "gtmpcnut", param = 0, threshold = 4.3) +
#   labs(x = "",
#        y = "Mean Monthly Chlorophyll-a (\U00B5g/L)",
#        title = "Pellicer Creek") +
#   annotate("text",
#            x = "Mar",
#            y = 38,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 4.3 (\U00B5g/L)")

## 04.3 Static Boxplots ----

# # CHLA
# ## PI
# boxplot_currentyear(station = "gtmpinut", 
#                     param = 1, 
#                     threshold = 0.65) +
#   labs(title = "Pine Island") +
#   annotate("text",
#            x = "Mar",
#            y = 1.2,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 0.65 (mg/L)")
# ## SS
# SS <- agm(station = "gtmssnut", 
#           param = 0, 
#           threshold = 4.0) +
#   labs(title = "San Sebastian") +
#   annotate("text",
#            x = "2006",
#            y = 6,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 4.0 (\U00B5g/L)")
# ## FM
# boxplot_currentyear(station = "gtmfmnut", 
#                     param = 0, 
#                     threshold = 5.5) +
#   labs(title = "Fort Matanzas") +
#   annotate("text",
#            x = "Mar",
#            y = 24,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 5.5 (\U00B5g/L)")
# ## PC
# PC <- agm(station = "gtmpcnut", 
#           param = 0, 
#           threshold = 4.3) +
#   labs(title = "Pellicer Creek") +
#   annotate("text",
#            x = "2006",
#            y = 16,
#            size = 3,
#            color = "blue",
#            label = "State Threshold 4.3 (\U00B5g/L)")
# 
# SS + (PC + labs(y = ""))


## 01.2 yearly agms ----

agm <- function(station, param, threshold) {
  if (param == 0) {
    agm <- NUT_yearly  %>% 
      dplyr::filter(STATION_CODE == station) %>% 
      ggplot(aes(x = YEAR, y = CHLA_agm, group = 1)) +
      geom_line() +
      geom_point(aes(color = CHLA_agm > threshold), size = 3) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", values = c("black", "red")) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(color = "black"),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.6)) +
      labs(x = "",
           y = "Geo.Mean Annual Chlorophyll-a (\U00B5g/L)")
  } else if (param == 1) {
    agm <- NUT_yearly  %>% 
      dplyr::filter(STATION_CODE == station) %>% 
      ggplot(aes(x = YEAR, y = TN_agm, group = 1)) +
      geom_line() +
      geom_point(aes(color = TN_agm > threshold), size = 3) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", values = c("black", "red")) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(color = "black"),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.6)) +
      labs(x = "",
           y = "Geo.Mean Annual Total Nitrogen (mg/L)")
  } else {
    agm <- NUT_yearly  %>% 
      dplyr::filter(STATION_CODE == station) %>% 
      ggplot(aes(x = YEAR, y = TP_agm, group = 1)) +
      geom_line() +
      geom_point(aes(color = TP_agm > threshold), size = 3) +
      geom_hline(yintercept = threshold, color = "blue",
                 linetype = "dashed") +
      scale_color_manual(name = "", values = c("black", "red")) +
      theme_classic() +
      theme(legend.position = "none",
            axis.text = element_text(color = "black"),
            axis.text.x = element_text(angle = 45, 
                                       vjust = 0.6)) +
      labs(x = "",
           y = "Geo.Mean Annual Total Phosphorus (mg/L)")
  }
}

# a <- agm(station = "gtmpcnut",
#     param = 0,
#     intercept = 4.3)
# a

##########################################################


# # 01 info ----
# # must run 01_load_wrangle.R first
# # use data frame dat3 for analysis and graphics
# 
# # ----02 figure function all stations----
# # run code, then use function.
# allstation_plot <- function(param, axis_y_title) {
#   
#   dat3 %>%
#     dplyr::filter(monitoringprogram == 1) %>%
#     dplyr::mutate(date = lubridate::date(date_sampled)) %>%
#     dplyr::select(station_code, date, cdmo_name, result) %>%
#     dplyr::group_by(station_code, date, cdmo_name) %>%
#     dplyr::summarise(result2 = mean(result, na.rm = TRUE)) %>%
#     dplyr::filter(cdmo_name == param) %>%
#     ggplot(aes(x = date, y = result2, color = station_code)) +
#     geom_point(size = 3) +
#     geom_line(size = 1, linetype = "dashed") +
#     scale_x_date(limits = c(as.Date("2019-01-01"), as.Date("2019-12-31")),
#                  date_breaks = "month", date_labels = "%b") +
#     theme_cowplot() +
#     labs(y = axis_y_title,
#          title = param)
# }
# 
# # ----03 figure function for thresholds----
# chla_threshold_plot <- function(station, threshold_low) {
#   
#   dat3 %>%
#     dplyr::filter(monitoringprogram == 1) %>%
#     dplyr::mutate(date = lubridate::date(date_sampled)) %>%
#     dplyr::select(station_code, date, cdmo_name, result) %>%
#     dplyr::group_by(station_code, date, cdmo_name) %>%
#     dplyr::summarise(result2 = mean(result, na.rm = TRUE)) %>%
#     dplyr::filter(cdmo_name == 'CHLA_N' & station_code == station) %>%
#     ggplot(aes(x = date, y = result2)) +
#     geom_point(size = 3) +
#     geom_line(size = 1) +
#     geom_hline(yintercept = threshold_low, linetype = "dashed", 
#                size = 2, color = "lightblue") +
#     geom_hline(yintercept = 20, linetype = "dashed", 
#                size = 2, color = "orange") +
#     theme_cowplot() +
#     labs(y = chla_y_title,
#          title = station,  
#          subtitle = paste('low threshold =', threshold_low))
# }
# 
# # ----04 figure function pellicer ISCO----
# # run code, then use function.
# isco_graph <- function(param, monthselect, axis_y_title) {
#   
#   # filters out ISCO data
#   # creates months as numbers
#   # selects only relevant parameters
#   # opens view to see data
#   View(dat3 %>%
#          dplyr::filter(monitoringprogram == 2) %>%
#          dplyr::mutate(month = lubridate::month(date_sampled)) %>%
#          dplyr::select(month, date_sampled, cdmo_name, result) %>%
#          dplyr::filter(month == monthselect & cdmo_name == param) %>%
#          tidyr::pivot_wider(id_cols = c('date_sampled'), 
#                             names_from = cdmo_name,
#                             values_from = result)
#   )
#   
#   # filters out ISCO data
#   # creates months as numbers
#   # selects only relevant parameters
#   dat3 %>%
#     dplyr::filter(monitoringprogram == 2) %>%
#     dplyr::mutate(month = lubridate::month(date_sampled)) %>%
#     dplyr::select(month, date_sampled, cdmo_name, result) %>%
#     dplyr::filter(month == monthselect & cdmo_name == param) %>%
#     ggplot(aes(x = date_sampled, y = result)) +
#     geom_point(size = 3) +
#     geom_line(size = 1, linetype = "dashed") +
#     scale_x_datetime(date_breaks = "4 hour", date_labels = "%b %d %I:%M") +
#     theme_cowplot() +
#     labs(y = axis_y_title,
#          title = param)
#   
# }
