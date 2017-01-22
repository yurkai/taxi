library(xts)
library(shiny)
library(shinydashboard)
library(leaflet)
library(lubridate)
library(highcharter)
library(ggplot2)
library(data.table)

regions_102 <- c(1075, 1076, 1077, 1125, 1126, 1127, 1128, 1129, 1130, 1131, 
                 1132, 1172, 1173, 1174, 1175, 1176, 1177, 1178, 1179, 1180, 
                 1181, 1182, 1183, 1184, 1221, 1222, 1223, 1224, 1225, 1227, 
                 1228, 1229, 1230, 1231, 1232, 1233, 1234, 1235, 1272, 1273, 
                 1274, 1278, 1279, 1280, 1281, 1282, 1283, 1284, 1285, 1286, 
                 1287, 1326, 1327, 1331, 1332, 1333, 1334, 1335, 1336, 1337, 
                 1338, 1339, 1376, 1377, 1378, 1380, 1382, 1383, 1384, 1385, 
                 1386, 1387, 1388, 1389, 1390, 1426, 1431, 1434, 1435, 1436, 
                 1437, 1438, 1439, 1441, 1442, 1480, 1482, 1483, 1530, 1532, 
                 1533, 1580, 1630, 1684, 1733, 1734, 1783, 2068, 2069, 2118, 
                 2119, 2168)

dt_pred <- readRDS('data_pred.rds')
regions <- readRDS('regions_102.rds')
data_full <- readRDS('data_june.rds')
reg <- 0

hc_empty <- highchart(type = "stock") %>%
    hc_title(text = paste("Select region and forecast date/hour")) %>%
    hc_scrollbar(enabled = FALSE) %>%
    hc_navigator(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_monokai())

body <- dashboardBody(background = "black",
    fluidRow(
        box(width = 12, background = "black",
            highchartOutput('chart', height = 180)
        ) 
    ),
    fluidRow(
        box(width = 4, background = "black",
            # вывод
            leafletOutput('map', height = 350) 
        ),
        box(width = 8, background = "black",
            # вывод
            # textOutput("text1"),
            fluidRow(column(6, sliderInput('fc_date', label = 'Date',
                                           min = min(dt_pred$date),
                                           max = max(dt_pred$date),
                                           value = min(dt_pred$date),
                                           timeFormat = '%b-%d',
                                           width = '92%'),
                            align="center"),
                     column(6, sliderInput('fc_hour', label = 'Hour',
                                           min = 0,
                                           max = 23,
                                           value = 23,
                                           width = '92%'),
                            align="center")
            ),
            fluidRow(column(12, 
                            highchartOutput('fc_plot', height = 250),
                            align="center")
            )
        )
    )

)

shinyApp(
    ui = dashboardPage(skin = 'black',
        dashboardHeader(title = "Yellow Taxi Project: Demo (yurkai)", 
                        titleWidth = 350),
        dashboardSidebar(
            disable = TRUE,
            tags$head(tags$style(HTML('.content-wrapper,
                        .right-side {background-color: #161616;}'))),
            tags$style(type = "text/css", "
                        .control-label {font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; 
                        font-size: 14px; color: #FBEEE6;font-weight: 300;}
                       .irs-grid-text {font-family: Arial; color: #F7F9F9;}
                       .irs-max {font-family: Arial; color: #F7F9F9;}
                       .irs-min {font-family: Arial; color: #F7F9F9;}
                       .irs-single {color:black; background: #ECF0F1;}
                        .irs-bar {background: #7B7D7D; border-color: #7B7D7D;}
                        .irs-bar-edge {background: #7B7D7D; border-color: #7B7D7D;}
                       ")
        ),
        body
    ),
    server = function(input, output, session) {
        
        # отладка - номер региона
        output$text1 <- renderText({
            paste(is.null(input$map_shape_click))
            # paste(click_marker())
        })
        
        # карта
        output$map <- renderLeaflet({
            # отображаем карту
            leaflet(regions) %>% addTiles() %>%
                addRectangles( 
                    lng1 = ~west, lng2 = ~east, lat1 = ~south, lat2 = ~north,
                    layerId = ~region,
                    # границы квадратиков
                    weight = 1, color = 'black', opacity = 2/10,
                    # заливка квадратиков
                    fillOpacity = 0.2, fillColor = 'gray')
        })
        
        # получаем регион из карты
        click_marker <- eventReactive(input$map_shape_click, {
            x <- input$map_shape_click
            return(x$id)
        })
        
        
        # хайчартер
        output$chart <- renderHighchart({
            if (is.null(input$map_shape_click)) return(hc_empty)
            
            reg <- click_marker()
            reg_xts <- xts(data_full[[grep(reg, colnames(data_full))]],
                           data_full[['date']])

            hc_plot <- highchart(type = "stock") %>%
                hc_title(text = paste("Region", reg, 'original trips')) %>%
                hc_add_series(reg_xts, id = paste('region', reg),
                              name = "trips") %>%
                hc_rangeSelector(inputEnabled = FALSE, enabled = FALSE) %>%
                hc_scrollbar(enabled = FALSE) %>%
                hc_navigator(enabled = FALSE) %>% 
                hc_add_theme(hc_theme_monokai())
            
            hc_plot
        })
        
        output$fc_plot<-renderHighchart({
            if (is.null(input$map_shape_click)) return(hc_empty)
            # параметры графика
            reg <- click_marker()
            hr <- input$fc_hour
            day <- input$fc_date
            
            if (input$fc_date == min(dt_pred$date) & input$fc_hour < 23){
                hr <- 23
                updateSliderInput(session, inputId = 'fc_hour', value = hr)
            }
            
            if (input$fc_date == max(dt_pred$date) & input$fc_hour > 17){
                hr <- 17
                updateSliderInput(session, inputId = 'fc_hour', value = hr)
            }
            
            # табличка для графика
            dt_dc <- dt_pred[region == reg & date == day & hour == hr, 
                             .(datetime, y)]
            dt_dc$n <- data_full[date %in% dt_dc$datetime,
                                 paste0('r', reg), with = FALSE][[1]]
            
            # хtsы
            xts_n <- xts(dt_dc$n, dt_dc$datetime)
            xts_pred <- xts(dt_dc$y, dt_dc$datetime)
            
            highchart(type = "stock") %>% 
                hc_title(text = paste("Trips forecast for region", reg)) %>%
                hc_add_series(xts_n, id = "n", name = 'target') %>% 
                hc_add_series(xts_pred, id = "pred", name = 'forecast') %>% 
                hc_rangeSelector(inputEnabled = FALSE, enabled = FALSE) %>%
                hc_scrollbar(enabled = FALSE) %>%
                hc_navigator(enabled = FALSE) %>% 
                hc_add_theme(hc_theme_monokai())
        })

    }
)