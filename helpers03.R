############################################################
# все файлы из folder_in обрабатываются и записываются 
# в folder_out. 

raw_to_agg <- function(verbose = TRUE) {
    # папки и паттерн
    folder_in <- 'data_in' 
    folder_out <- 'data_out'
    yt_pattern <- 'yellow_tripdata_'
    
    # засечем время
    start <- now()
    
    # имена файлов с данными
    files_in <- list.files(paste(getwd(), folder_in, sep = '/'))
    files_in <- files_in[grep(yt_pattern, files_in)]
    files_out <- sub(yt_pattern, '', files_in)
    
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
        dt_agg <- get_dt_agg_with_zeroes(dt_month) # агрегация по регионам
        # исправим время и удалим ненужные колонки
        dt_agg[, `:=`(date = paste(year, month, day, hour) %>% ymd_h,
                      year = NULL, month = NULL, day = NULL, hour = NULL)]
        fwrite(dt_agg, filename_out)
        if (verbose) cat(' ... ок')
    }
    
    if (verbose) paste('\nвремя обработки', 
                       round(as.numeric(now() - start, units = 'mins'), 1), 
                       'минут') %>% cat
    
}




############################################################
# читаются зоны regions_to_read из файлов в папки folder_out

read_regions <- function(regions_to_read, verbose = TRUE) {
    
    folder <- 'data_out'
    files <- list.files(paste(getwd(), folder, sep = '/')) %>%
        paste(getwd(), folder, ., sep = '/')
    dt <- data.table()
    
    # засечем время
    start <- now()
    
    if (verbose) paste('файлов:', length(files), '\n') %>% cat
    
    for (filename in files){
        cat('. ')
        dt_agg <- fread(filename)
        dt_agg[, date := parse_date_time(date, "YmdHMS")]
        dt_agg <- dt_agg[region %in% regions_to_read] %>% 
            dcast(date ~ region, value.var = 'n')
        dt <- rbindlist(list(dt, dt_agg), use.names = TRUE)
        
    }
    
    setkey(dt, date)
    
    if (verbose) paste('\nвремя обработки', 
                       round(as.numeric(now() - start, units = 'secs'), 1), 
                       'сек') %>% cat
    
    dt
}




############################################################
# создаем регрессионные признаки из синусов и косинусов
# k -- количество синусов/косинусов
# from, to -- номер периода
# fun - функция для тренда
# trend -- включать ли тренда
# weeks -- включать ли недельные признаки
# years -- включать ли годовые признаки

get_xreg <- function(k = 2, from = 1, to, fun = function(x) x,
                     trend = FALSE, weeks = TRUE, years = FALSE) {
    
    dt <- data.table(idx = from:to)
    
    # добавляем тренд
    if (trend) dt <- dt[,trend := fun(idx)] 
    
    for (i in 1:k){
        vars_week <- paste0(c('week_s_', 'week_c_'), i)
        vars_year <- paste0(c('year_s_', 'year_c_'), i)
        
        # добавляем недели
        if (weeks){
            dt[, vars_week[1] := sin(i * idx * 2*pi / 168)]
            dt[, vars_week[2] := cos(i * idx * 2*pi / 168)]
        }
        
        # добавляем годы
        if (years){
            dt[, vars_year[1] := sin(i * idx * 2*pi / 8766)]
            dt[, vars_year[2] := cos(i * idx * 2*pi / 8766)]
        }
    }
    
    # сортируем колонки по именам, чтобы удобнее смотреть
    setcolorder(dt, colnames(dt) %>% sort)
    
    dt[, -c('idx')]
}




############################################################
# в связи с новым жжплотом сейчас (12/2016) не отображается
# pacf в autoplot. поэтому придется переписать функцию
# построения графиков, чтобы все было в одном стиле
# расчет интервалов взят из stats::plot_acf

plot_acf_pacf <- function(my_ts, max_lag = 36, ci = 0.95, fill_clr = 'coral'){
    # получаем автокорреляционные функции
    my_acf <- acf(my_ts, lag.max = max_lag, plot = FALSE)
    my_pacf <- pacf(my_ts, lag.max = max_lag, plot = FALSE)
    # рассчитываем доверительные интервалы
    ci <- 0.95
    clim0 <- qnorm((1 + ci)/2)/sqrt(my_acf$n.used)
    clim_acf <- clim0 * sqrt(cumsum(2 * my_acf$acf[-1, 1, 1]^2))
    clim_pacf <- clim0 * sqrt(cumsum(2 * my_pacf$acf[, 1, 1]^2))
    
    # табличка с интервалами
    dt_ci <- data.table(lag = rep(1:max_lag, 2), ci = c(clim_acf, clim_pacf),
                        variable = c(rep('acf', max_lag), rep('pacf', max_lag)))
    
    # таблица со значениями acf/pacf
    dt <- data.table(
        lag = 1:max_lag, 
        acf = tail(my_acf$acf, -1),
        pacf = my_pacf$acf[,1,1]) %>% 
        melt(id.vars = 'lag', value.var = c('acf', 'pacf'))

    # сам график
    ggplot(dt, aes(x = lag, y = value)) + 
        facet_grid(variable~., switch = 'y') +
        geom_ribbon(data = dt_ci, aes(x = lag, y= 0, ymin = -ci, ymax = ci), 
                    fill = fill_clr, alpha = 0.3) + 
        geom_point(data = dt, aes(x = lag, y = value), size = .5) + 
        geom_errorbar(data = dt, aes(x = lag, ymin = 0, ymax = value),
                      width = 0.1) + 
        labs(y = '', title = '')
}




############################################################
# дамми для дня недели (auto.arima не понимает факторы)

get_wday_dummy <- function(date_time, drop_first = TRUE){
    cols <- ifelse(drop_first, 2, 1):7 # номера колонок
    (data.table(day = factor(wday(date_time), labels = 1:7)) %>%
            model.matrix(~ 0 + day, .))[,cols]
}

