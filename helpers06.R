# лаги и липы для праздников
make_shifts <- function(dt, lags_d = 1, leads_d = 1){
    if (all(lags_d != 0)) {
        lags <- dt[, shift(.SD, lags_d * 24,
                           give.names = TRUE), .SDcols = 1]
        dt <- cbind(dt, lags)
    }
    if (all(leads_d != 0)) {
        leads <- dt[, shift(.SD, leads_d * 24, 
                            give.names = TRUE, type = 'lead'), .SDcols = 1]
        dt <- cbind(dt, leads)
    }
    
    replace_NA_by_value(dt)
    
    dt[,-1]
}


# замена NA, NaN, Inf на какие-то значения/нули в дата.тейбле
# работает по ссылке, не возвращает данные
replace_NA_by_value <- function(DT, value = 0) {
    # for (i in names(DT))
    #     DT[is.na(get(i)), i := value, with=FALSE]
    for (j in seq_len(ncol(DT)))
        set(DT, which(is.na(DT[[j]])), j, value)
}



# праздники и события с лагами и лидами
get_holidays <- function(tbl) {
    years <- tbl[, year(date) %>% unique]
    
    # new year
    tbl[, new_year := 0]
    tbl[date(date) %in% (holiday.NewYears(years) %>% mdy), new_year := 1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(new_year)], 1:3, 1:3))
    
    # martin luter king
    tbl[, mlk := 0]
    tbl[date(date) %in% (holiday.MLK(years) %>% mdy), mlk := 1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(mlk)], 1, 0))
    
    # st_patric
    tbl[, st_patrics := 0]
    tbl[date(date) %in% (holiday.StPatricks(years) %>% mdy), st_patrics := 1]
    
    # easter
    tbl[, easter := 0]
    tbl[date(date) %in% (holiday.Easter(years) %>% mdy), easter := 1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(easter)]))
    
    # memorial
    tbl[, memorial := 0]
    tbl[date(date) %in% (holiday.Memorial(years) %>% mdy), memorial := 1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(memorial)]))
    
    # independence
    tbl[, independence := 0]
    tbl[date(date) %in% (holiday.Independence(years) %>% mdy), independence:=1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(independence)], 1:2, 1:2))
    
    # labor
    tbl[, labor := 0]
    tbl[date(date) %in% (holiday.Labor(years) %>% mdy), labor := 1]
    
    # columbus
    tbl[, columbus := 0]
    tbl[date(date) %in% (holiday.Columbus(years) %>% mdy), columbus := 1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(columbus)], 1, 0))
    
    # veterans
    tbl[, veterans := 0]
    tbl[date(date) %in% (holiday.Veterans(years) %>% mdy), veterans := 1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(veterans)], 1, 0))
    
    # thanksgiving
    tbl[, thanksgiving := 0]
    tbl[date(date) %in% (holiday.Thanksgiving(years) %>% mdy), thanksgiving:=1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(thanksgiving)], 1:2, 1))
    
    # christmas
    tbl[, christmas := 0]
    tbl[date(date) %in% (holiday.Christmas(years) %>% mdy), christmas:=1]
    tbl <- cbind(tbl, make_shifts(tbl[,.(christmas)], 1:3, 1:2))
    
    # чтобы убрать аномалии
    # blizzards 2015, 2016
    blizzards <- c('2016-01-23', '2015-01-26', '2015-01-27') %>% ymd
    tbl[, blizzard := 0]
    tbl[date(date) %in% blizzards, blizzard := 1]
    
    tbl[, -1]
}



#####################################################################
# агрегируем данные по поездкам и средними продолжительностью, кол-ом 
# пассажиров, расстоянием и стоимостью

get_dt_agg_aux <- function(dt){
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
    dt_agg <- dt[CJ(day, hour, region, unique = TRUE), 
                 .(n = .N,
                   avr_dur = mean(duration, na.rm = TRUE),
                   avr_psngr = mean(passenger_count, na.rm = TRUE),
                   avr_dist = mean(trip_distance, na.rm = TRUE),
                   avr_tax = mean(total_amount, na.rm = TRUE)),
                 by = .EACHI]
    # надо обнулить частоты для искусственно введенных значений
    dt_agg[(day %in% missing_days) | 
               (hour %in% missing_hours) | 
               (region %in% missing_regions), 
           `:=`(n = 0,
                avr_dur = 0,
                avr_psngr = 0,
                avr_dist = 0,  
                avr_tax = 0)]
    
    # добавляем месяц в получившуюся таблицу
    dt_agg[, `:=`(month = month(first_date),
                  year = year(first_date))]
    
    replace_NA_by_value(dt_agg)
    
    # устанавливаем красивый порядок колонок
    setcolorder(dt_agg, c('year', 'month', 'day','hour', 'region', 'n',
                          'avr_dur', 'avr_psngr', 'avr_dist', 'avr_tax' ))
    
    dt_agg[]
}



############################################################
# прочитаем все файлы с дополнительными данными

read_aux_regions_102 <- function(verbose = TRUE) {
    
    folder <- 'data_out'
    files <- list.files(paste(getwd(), folder, sep = '/')) %>%
        paste(getwd(), folder, ., sep = '/')
    files <- files[grep(pattern = 'aux_', files)]
    dt <- data.table()
    
    # засечем время
    start <- now()
    
    if (verbose) paste('файлов:', length(files), '\n') %>% cat
    
    for (filename in files){
        cat('. ')
        dt_agg <- fread(filename)
        dt_aggсмы[, date := parse_date_time(date, "YmdHMS")]
        print(all(colnames(dt) == colnames(dt_agg)))
        dt <- rbindlist(list(dt, dt_agg), use.names = TRUE)
    }
    
    setkey(dt, date)
    
    if (verbose) paste('\nвремя обработки', 
                       round(as.numeric(now() - start, units = 'secs'), 1), 
                       'сек') %>% cat
    
    dt[]
}




####################################################################
# все файлы из folder_in обрабатываются и записываются в folder_out

raw_to_agg_aux_102 <- function(verbose = TRUE) {
    # папки и паттерн
    folder_in <- 'data_in' 
    folder_out <- 'data_out'
    yt_pattern <- 'yellow_tripdata_'
    
    # засечем время
    start <- now()
    
    # имена файлов с данными
    files_in <- list.files(paste(getwd(), folder_in, sep = '/'))
    files_in <- files_in[grep(yt_pattern, files_in)]
    files_out <- paste0('aux_', sub(yt_pattern, '', files_in))
    
    if (verbose) paste('файлов:', length(files_in)) %>% cat
    
    # загружаем файлы и сохраняем обработанные
    for (i in seq_along(files_in)) {
        # с какими файлами работаем
        filename_in <- paste(getwd(), folder_in, files_in[i], sep = '/')
        filename_out <- paste(getwd(), folder_out, files_out[i], sep = '/')
        if (verbose) paste0('\nфайл[', i, ']: ', files_in[i]) %>% cat
        
        # обработка
        dt_month <- fread(filename_in) # чтение
        dt_month <- clear_data(dt_month) # очистка таблицы
        dt_month <- make_regions(dt_month) # присвоение регионов
        dt_agg <- get_dt_agg_aux(dt_month) # агрегируем с доп признаками
        # исправим время и удалим ненужные колонки
        dt_agg[, `:=`(date = paste(year, month, day, hour) %>% ymd_h,
                      year = NULL, month = NULL, day = NULL, hour = NULL)]
        # оставляем только 102 региона
        dt_agg <- dt_agg[region %in% regions_102]
        value_vars <- setdiff(colnames(dt_agg), c('date', 'region'))
        dt_agg <- dt_agg %>% dcast(date ~ region, value.var =value_vars)
        
        fwrite(dt_agg, filename_out)
        if (verbose) cat(' ... ок')
    }
    
    if (verbose) paste('\nвремя обработки', 
                       round(as.numeric(now() - start, units = 'mins'), 1), 
                       'минут') %>% cat
    
}



###############################################################################
# рассчитаем индивидуальные для каждой признаки для текущего горизонта прогноза

make_data <- function(j = 1){
    # таблица для признаков
    data <- data.table()
    for (reg in seq_along(regions_102)){
        # cat(regions_102[reg])
        # доп признаки ср длительность, пассажиры, дистанция и сумма
        # исходные
        auxs <- data_2016[, c(paste0(features_aux, regions_102[reg])), 
                          with=FALSE]
        # список таблиц
        auxs <- lapply(colnames(auxs), function(feat){
            dt <- make_lags_and_sums(auxs[,..feat], lags_aux)[,-1]
            colnames(dt) <- paste(substr(feat ,1, nchar(feat)-5), 
                                  colnames(dt), sep = '_')
            dt
        })
        # теперь все в одной
        auxs <- do.call('cbind', auxs)
        
        # предсказания предыдущей модели
        latter_model <- week_05_pred[order(date)][
            region == regions_102[reg] & h == j, .(latter_model = y)]
        # latter_model <- make_lags_and_sums(data_full[,-1][, ..reg], 
        #                                    lags_latter_model)[,-1]
        latter_model <- 
            make_lags_and_sums(latter_model, lags_latter_model)[,-1]
        colnames(latter_model) <- paste0('lm_', colnames(latter_model) )
        
        # лаги и суммы целевой переменной
        lags_n_sums <- make_lags_and_sums(data_full[,-1][, ..reg], lags)[,-1]
        # целевая переменная
        ys <- data_full[,-1][, shift(.SD, n = hs, type = 'lead'), .SDcols=reg]
        colnames(ys) <- paste0('y_', hs)
        
        # добавим регион
        region <- paste0('r', regions_102[reg])
        
        # соединим все фичи
        ind_features <- cbind(region, ys, latter_model, lags_n_sums, 
                              auxs, common_features)
        
        # первый расчет?
        if (reg == 1) {
            data <- copy(ind_features)
        } else {
            data <- rbindlist(list(data, ind_features))
        }
    }
    
    # регионы в факторы и готово
    data[, region := as.factor(region)][]
}
