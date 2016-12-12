############################################################
# очистка данных

clear_data <- function(dt){
    dt <- make_durations(dt) # вводим длительность в сек
    dt <- drop_0_duration(dt) # 
    dt <- drop_0_passenger_count(dt)
    dt <- drop_0_trip_distance(dt)
    dt <- drop_out_of_NY(dt)
    
    dt
}




############################################################
# новый признак -- продолжительность поездки

make_durations <- function(dt){
    # время посадки высадки -- character
    dt[,`:=`(
      tpep_pickup_datetime = parse_date_time(tpep_pickup_datetime, "YmdHMS"),
      tpep_dropoff_datetime = parse_date_time(tpep_dropoff_datetime, "YmdHMS"))]
    dt[, duration := as.numeric(tpep_dropoff_datetime - tpep_pickup_datetime,
                                units = "secs")]
    setkey(dt, tpep_pickup_datetime)
    
    dt
}



############################################################
# удалить строки с duration == 0

drop_0_duration <- function(dt){
    dt[duration != 0]
}



############################################################
# удалить строки с passenger_count == 0

drop_0_passenger_count <- function(dt){
    dt[passenger_count != 0]
}



############################################################
# удалить строки с trip_distance == 0

drop_0_trip_distance <- function(dt){
    dt[trip_distance != 0]
}



############################################################
# удалить строки с координатами начала, не попадающими в прямоугольник Нью-Йорка

drop_out_of_NY <- function(dt){
    dt[pickup_longitude >= NY$lon[1] & pickup_longitude <= NY$lon[2] &
       pickup_latitude >= NY$lat[1] & pickup_latitude <= NY$lat[2]]
}



############################################################
# определение ячейки по координатам

get_region <- function(lon, lat){
    lon_cell <- findInterval(lon, NY$lon_breaks, rightmost.closed=TRUE)
    lat_cell <- findInterval(lat, NY$lat_breaks, rightmost.closed=TRUE)
    
    (lon_cell - 1) * NY$CELLS + lat_cell
}

# 
make_regions <- function(dt){
    dt[, region := get_region(pickup_longitude, pickup_latitude)]
}



############################################################
# 

get_dt_agg_with_zeroes <- function(dt){
    # добавим час и день (потом еще надо будет месяц)
    dt[, `:=`(day = mday(tpep_pickup_datetime),
                    hour = hour(tpep_pickup_datetime))]
    first_date <- dt[1, tpep_pickup_datetime]
    
    # узнаем есть ли пропуски по датам
    days <- days_in_month(first_date) %>% seq_len
    missing_days <- setdiff(days, dt[, unique(day)])
    if (length(missing_days) > 0){
        # добавляем эти дни в данные
        dt_missing <- data.table(region = 1, day = missing_days, hour = 0)
        dt <- rbindlist(list(dt, dt_missing), fill = TRUE)
    }
    
    # узнаем есть ли пропуски по часам
    missing_hours <- setdiff(1:23, dt[, unique(hour)])
    if (length(missing_hours) > 0){
        dt_missing <- data.table(region = 1, day = 1, hour = missing_hours)
        dt <- rbindlist(list(dt, dt_missing), fill = TRUE)
    }
    
    # узнаем есть ли пропуски по регионам
    missing_regions <- setdiff(seq_len(nrow(regions)), dt[, unique(region)])
    if (length(missing_regions) > 0){
        dt_missing <- data.table(region = missing_regions, day = 1, hour = 0)
        dt <- rbindlist(list(dt, dt_missing), fill = TRUE)
    }
    
    # делаем сводную
    setkey(dt, day, hour, region)
    dt_agg <- dt[CJ(day, hour, region, unique = TRUE), .(n = .N), by = .EACHI]
    # надо обнулить частоты для искусственно введенных значений
    dt_agg[(day %in% missing_days) | 
           (hour %in% missing_hours) | 
           (region %in% missing_regions), n := 0]
    
    # можно проверить, что добавляем все правильно:
    # никаких левых данных мы не записали
    # dt_agg[(region %in% missing_regions), unique(region)] %>%
    #     setdiff(., missing_regions)
    # 
    # количество строк правильное: дней_в_месяце * часов_в_дне * число_регионов
    # nrow(dt_agg) == 
    #     nrow(regions) * days_in_month(dt[1, tpep_pickup_datetime]) * 24
    
    # добавляем месяц в получившуюся таблицу
    dt_agg[, `:=`(month = month(first_date),
                  year = year(first_date))]
    
    # устанавливаем красивый порядок колонок
    setcolorder(dt_agg, c('year', 'month', 'day','hour', 'region', 'n'))
    
    # если не жалко время, можно вернуть данные к исходному числу строк
    # dt <- dt[!((day %in% missing_days) |
    #                            (hour %in% missing_hours) | 
    #                            (region %in% missing_regions))]
    # dt[,`:=`(day = NULL, hour = NULL)]
    
    setkey(dt_agg, year)
    
    dt_agg
}






