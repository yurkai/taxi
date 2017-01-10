# ARIMA(p,d,q)(P,D,Q)[per]  
# параметры моделей
nice_arima_print <- function(fit){
    info <- setNames(fit$arma, c("p", "q", "P", "Q", "m", "d", "D"))
    paste0('ARIMA(', info[1], ',', info[6], ',', info[2], ')(',
           info[3], ',', info[7], ',', info[4], ')[', info[5], ']')
}



# прогноз на 1-h часов
predict_h <- function(fit, ts_fit, xreg_fit, xreg_h, h = 6){
    assert_that(h == nrow(xreg_h))
    assert_that(length(ts_fit) == nrow(xreg_fit))
    
    # рефит
    refit <- Arima(ts_fit, model=fit, xreg = xreg_fit)
    
    # прогноз
    fc <- forecast(refit, xreg = xreg_h)$mean
    # ограничиваем снизу нулем
    fc[fc < 0] <- 0
    
    fc
}



# кусочек строки, где часы одна или две цифры
get_subm_ymdh <- function(timedate){
    paste(format(timedate, '%F'), hour(timedate), sep = '_')
}



# получить конец истории как в end(td) 
# n - число наблюдений, freq - частота
get_end <- function(n, freq = 24){
    c((n-1) %/% 24 + 1, (n-1) %% 24 + 1)
}



# получить предсказания для текущего региона на 1:h шагов
# data_full -- глобальная
# freq -- глобальная (=24)
get_pred_region_h <- function(region, fit, start_date, end_date, end_date_test,
                              h = 6){

    # ts_reg <- полный ряд по региону (с тестовыми данными в том числе)
    ts_reg <- ts(data_full[date >= start_date, region, with = FALSE][[1]], 
                 start = 1, frequency = 24)
    # находим конец первой истории
    end_iter <- get_end(data_full[date >= start_date & date <= end_date, .N])
    
    # определяем количество историй
    n_max <- data_full[date >= end_date & date <= end_date_test, .N]
    
    # таблица для результатов
    predict_table <- data.table(id = character(), y = numeric())
    
    # всего нам потребуется вот столько шагов для регрессоров
    n_xreg_steps <- data_full[date >= start_date & 
                                  date <= end_date_test, .N] + h
    xreg <- get_xreg(n_xreg, 1, n_xreg_steps)
    
    # для каждого конца истории считаем от 1 до h предсказаний
    for (n in seq_len(n_max)){
        # дата конца текущей истории
        end_hist <- data_full[which(data_full[,date] == end_date)-1 + n, date]
        
        # ряд-итератор
        ts_iter <- window(ts_reg, end = end_iter[1] + 
                              (end_iter[2]-1 + n-1)/freq)
        xreg_fit <- xreg[1:length(ts_iter)]
        xreg_h <- xreg[(length(ts_iter)+1):(length(ts_iter)+h)]
        
        # считаем прогноз
        pred_p <- predict_h(fit, ts_iter, xreg_fit, xreg_h)
        
        # добавляем к таблице прогнозов
        pred_hist <- data.table(id = paste(substring(region, 2),
                                           get_subm_ymdh(end_hist),
                                           1:h, sep = '_'),
                                y = as.numeric(pred_p))
        
        predict_table <- rbindlist(list(predict_table, pred_hist))
    }
    
    predict_table
}



# посчитать функционал ошибки
# data_full -- глобальная
get_Q <- function(pred_table){
    # делаем дату и регион из id
    tmp <- pred_table[, tstrsplit(id, '_')] %>% cbind(pred_table[,.(id)])
    tmp[, region := paste0('r', V1)]
    tmp[, date := ymd_h(paste(V2, V3)) + hours(V4)]
    
    # таблица с правильными ответами
    lookup_table <- melt(data_full, id.vars = 1, 
                         measure.vars = 2:ncol(data_full),
                         variable.name = "region", value.name = "n")[
                             date %in% tmp[, unique(date)]]
    
    # теперь в таблице есть и предсказания, и правильные ответы
    tmp <- merge(tmp, lookup_table, by = c('date', 'region'))
    tmp <- merge(tmp, pred_table, by = c('id'))
    
    # считаем MAE
    tmp[, mean(abs(n - y))]
}















