# Желтое такси в Нью-Йорке

Финальный проект специализации МФТИ и Яндекса "Машинное обучение и анализ данных"


### О проекте

Задача этого проекта — научиться предсказывать количество поездок в ближайшие часы в каждом районе Нью-Йорка. Для того, чтобы её решить, сырые данные необходимо агрегировать по часам и районам. Агрегированные данные будут представлять собой почасовые временные ряды с количествами поездок из каждого района. Похожие задачи возникают на практике, если вам необходимо спрогнозировать продажи большого количества товаров в большом количестве магазинов, объём снятия денег в сети банкоматов, посещаемость разных страниц сайта и т.д.

Помимо прогнозирования количества поездок из каждой активной геозоны Нью-Йорка необходимо представить интерактивное демо проекта с наглядным отображением результатов модели (прогноз на 1-6 часов).


### Что здесь есть

Первые 6 недель посвящены исследованию и обработке данных, отображению информации на карте, построению моделей ARIMA, линейной регрессии и выбору финальной модели (градиентный бустинг). Неделя 7 -- демо.

* [Неделя 1](https://cdn.rawgit.com/yurkai/taxi/1571784b/taxi01.html) -- знакомство с данными и их обработка,
* [Неделя 2](https://cdn.rawgit.com/yurkai/taxi/1571784b/taxi02.html) -- работа с геоданными,
* [Неделя 3](https://cdn.rawgit.com/yurkai/taxi/1571784b/taxi03.html) -- базовая модель ARIMA,
* [Неделя 4](https://cdn.rawgit.com/yurkai/taxi/1571784b/taxi04.html) -- кластеризация географических зон и ARIMA для каждой из них,
* [Неделя 5](https://cdn.rawgit.com/yurkai/taxi/3f3c62e3/taxi05.html) -- линейная модель,
* [Неделя 6](https://cdn.rawgit.com/yurkai/taxi/1571784b/taxi06.html) -- финальная модель (градиентный бустинг),
* [Неделя 7](https://beta.rstudioconnect.com/content/2443/) -- лаконичная демонстрация результатов проекта.

Пс. пожалуйста, [сообщите](mailto:isakovuv@gmail.com) на если демонстрация перестала работать (она была загружена на rstudiobconnect во время бета-тестирования).
