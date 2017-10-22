# mae
mae <- function(y, pred){
    abs(y - pred) %>% mean
}


# возвращает mae для каждого значения lambda на train/test
# fit -- модель glmnet
get_maes <- function(fit){
    lambdas <- fit$lambda
    train_pred <- predict(fit, newx=X[idx_train,], s = fit$lambdas) %>% 
        data.table
    test_pred <- predict(fit, newx=X[idx_test,], s = fit$lambdas) %>% 
        data.table
    mae_train <- sapply(train_pred, function(y_pred) 
        mae(y[idx_train], y_pred)) %>% as.numeric
    mae_test <- sapply(test_pred, function(y_pred) 
        mae(y[idx_test], y_pred)) %>% as.numeric
    
    data.table(lambda = lambdas, train = mae_train, test = mae_test)
}


# возвращает mae для каждого значения lambda на train/test
# fit -- модель glmnet
# new_y - таблица с будущим
get_maes_h <- function(fit, h){
    
    lambdas <- fit$lambda
    train_pred <- predict(fit, newx=X[idx_train,], s = fit$lambdas) %>% 
        data.table
    test_pred <- predict(fit, newx=X[idx_test,], s = fit$lambdas) %>% 
        data.table
    
    mae_train <- sapply(train_pred, function(y_pred) 
        mae(new_y[[h]][idx_train], y_pred)) %>% as.numeric
    mae_test <- sapply(test_pred, function(y_pred) 
        mae(new_y[[h]][idx_test], y_pred)) %>% as.numeric
    
    data.table(lambda = lambdas, train = mae_train, test = mae_test)
}


# ошибка от лямбды
plot_maes <- function(maes){
    maes %>% melt(id.vars = 1, measure.vars = 2:3) %>%
        ggplot(aes(x = lambda, y = value, color = variable)) + 
        geom_point(size = 0.3) + geom_line() +
        scale_x_log10(
            breaks = scales::trans_breaks("log10", function(x) 10^x),
            labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) + 
        labs(title = 'MAE на обучающей и  тестовой выборках',
             x = expression(lambda), y = 'MAE') + 
        scale_colour_manual(name = '', 
                            values =c(my_pal[1], my_pal[3]), 
                            labels = c('Обучающая', 'Тренировочная')) +
        theme(legend.position="bottom")
}


# подбираем альфу и лямбду
find_alpha_lambda <- function(alphas =seq(0, 1, by = 0.1),
                              lambdas = 10**seq(-5, 5, length.out = 50)){
    maes <- data.table()
    for (alpha in alphas){
        fit <- glmnet(X[idx_train,], y[idx_train], 
                      alpha = alpha, lambda = lambdas)
        maes <- rbindlist(list(maes, cbind(alpha = alpha, get_maes(fit)) ))
    }
    
    maes[test == min(test)][1]
}


# подбираем альфу и лямбду для модели с h
find_alpha_lambda_h <- function(h,
    alphas =seq(0, 1, by = 0.1), lambdas = 10**seq(-5, 5, length.out = 50)){
    
    maes <- data.table()
    
    for (alpha in alphas){
        fit <- glmnet(X[idx_train,], new_y[[h]][idx_train], 
                      alpha = alpha, lambda = lambdas)
        maes <- rbindlist(list(maes, cbind(alpha = alpha, get_maes_h(fit, h))))
    }
    
    fit <- glmnet(X[idx_train,], new_y[[h]][idx_train], 
                  alpha = maes[test == min(test)][1, alpha], 
                  lambda = maes[test == min(test)][1, lambda])
    
    list(info = cbind(h = h, maes[test == min(test)][1]), model = fit)
}


# новые признаки: лаги и суммы за предыдущие периоды
# выполняется для первой колонки
make_lags_and_sums <- function(tbl, lags = 0:6, sums = c(6, 12, 24, 168)){
    names_lags <- paste('lag', lags, sep = '_')
    names_sums <- paste('sum', sums, sep = '_')
    tbl[, c(names_lags) := shift(.SD, lags), .SDcols = 1]
    tbl[, c(names_sums) := 
            lapply(sums, function(s) Reduce(`+`, shift(.SD, 1:s))),
        .SDcols = 1]
    
    # заменим NA на среднее
    col_NA <- colnames(tbl)[colSums(is.na(tbl)) > 0]
    tbl[, (col_NA) := lapply(col_NA, function(x) {
        x <- get(x)
        x[is.na(x)] <- mean(x, na.rm = TRUE)
        x
    })]
    
    tbl[]
}



  
# строим прогноз по моделям glmnet
# data_full -- глобальная
# n_xreg -- глобальная (как и на обучении)
# xreg -- глобальная (как и на обучении)
# data_time -- глобальная (как и на обучении)
get_pred_table_h <- function(models, idx, h = 6){
    # итераторы
    is <- seq_along(regions_102)

    tbl_pred <- list()
    
    for (i in is){
        # cat(regions_102[i])
        
        # формируем лаги для каждого региона
        lags_n_sums <- make_lags_and_sums(data_full[,-1][, ..i])[,-1]
        data <- cbind(data_time, xreg, lags_n_sums)
        X <- model.matrix(~ . + wday:hour + hour:month -date , data)
        new_y <- data_full[,-1][,..i][
            , shift(.SD, 1:h, type = 'lead'), .SDcols = 1]
        
        # временный список с h таблицами региона
        tmp <- list()
        # прогноз на 1-h часов
        for (j in 1:h) {
            # достаем модель
            fit <- models[[paste0('r', regions_102[i])]][[j]]
            # прогноз
            pred <- predict(fit, newx=X[idx,], s = fit$lambda)[,1]
            # сохраняем прогноз в таблицу и добавляем id
            tmp[[j]] <- data.table(
                id = paste(regions_102[i], get_subm_ymdh(data[idx, date]), 
                           j, sep = '_'),
                y = pred)
        }
        # разворачиваем таблицу по региону и добавляем в список
        tbl_pred[[i]] <- rbindlist(tmp)
        
    }
    # разворачиваем таблицы регионов
    tbl_pred <- rbindlist(tbl_pred)
    
    tbl_pred
}









