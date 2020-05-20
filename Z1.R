#Ларкина Людмила 125 ПАЭ 
#для региона 45 рассчитайте урожайность пшеницы в 2017 году, взяв для рассчета средние суммы активных температур за предыдущие 14 лет, с 5 ближайших метеостанций
#Настройка и проверка рабочей директории
getwd()
#Подключим нужные пакеты
#библиотека( rnoaa)
library(rnoaa)
#библиотека( tidyverse)
library(tidyverse)
#библиотека (lubridate)
library (lubridate)
#Создание векторов с данными для рассчётов
#коэффициент для экпозиции склона - считаем что все поля идеально ровные
y  =  1.0
#Константы
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
#отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "Сумма частей основной и побочной продукции
Lj = 2.2
#Коэффициент "Стандартная влажность культуры"
Ej = 25
# 1. Скачивание СПИСКА МЕТЕОСТАНЦИЙ 
station_data = ghcnd_stations()
#Может занять несколько минут, лучше выполнить один раз в месте с хорошим интернетом 
#и сохранить результат
write.csv(station_data, "station_data.csv")
station_data  = read.csv ("station_data.csv")
#2. ФОРМИРОВАНИЕ СПИСКА МЕТЕОСТАНЦИЙ
#После получения списка всех станций, выберите из него список станций ближайших к 
#столице вашего региона,создав таблицу с именем региона и координатами его столицы
#координаторы должны быть в десятых градусов
kurgan = data.frame(id = "KURGAN", latitude = 55.4667,  longitude = 65.4000)
#прочитайте справку команды meteo_nearby_stations
meteo_nearby_stations
#можно выбирать метеостанции в некотором фиксированном радиусе от Кургана
#или конечное число станций, которые имеют необходимые данные
#в заданный временной период, и выбрать переменные, которые обязательно должны быть в наличии
kurgan_around  = meteo_nearby_stations(lat_lon_df = kurgan, station_data = station_data,limit = 5, var = c("PRCP", "TAVG"),year_min = 2002, year_max = 2016)
#kurgan_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций, отсортированных по их удаленности от Кургана 
#очевидно что первым элементом таблицы будет идентификатор метеостанции Кургана, 
#его то мы и попытаемся получить
kurgan_id = kurgan_around[["KURGAN"]][["id"]][1]
summary(kurgan_id)
kurgan_id
# 3.Скачивание погодных данных для выбранных метеостанций
#Создадим объект, куда скачаем все данные всех метеостанций
all_kurgan_meteodata = data.frame()
#Создадим цикл, чтобы скачать все данные со всех 5 метеостанций
for(i in 1:5) 
{ 
  kurgan_id = kurgan_around[["KURGAN"]][["id"]][i]
  data = meteo_tidy_ghcnd(stationid = kurgan_id,
                          var = "TAVG",
                          date_min = "2002-01-01",
                          date_max = "2016-12-31")
  all_kurgan_meteodata = bind_rows(all_kurgan_meteodata, data)
}
#Записываем полученные результаты
write.csv(all_kurgan_meteodata,file = "all_kurgan_meteodata.csv")
#Cчитываем данные из файла
all_kurgan_meteodata=read.csv("all_kurgan_meteodata.csv") 
# 4.Работа с полученными данными
#Создадим колонки year, month для группировки, выберем данные  для 2002-2016 годов
all_kurgan_meteodata=mutate(all_kurgan_meteodata, year=year(date),month=month(date),day=day(date))
#Посмотрим на данные
str(all_kurgan_meteodata)
#Отфильтруем данные за 2002-2016 год
years_kurgan_meteodata=filter(all_kurgan_meteodata, year %in% c(2002:2016))
#Проверим результат
str(years_kurgan_meteodata)
summary(years_kurgan_meteodata)
years_kurgan_meteodata[,"tavg"]=years_kurgan_meteodata$tavg/10
summary(years_kurgan_meteodata)
#Превратим в нули все NA и где  tavg <5
years_kurgan_meteodata[is.na(years_kurgan_meteodata$tavg),"tavg"] = 0
years_kurgan_meteodata[years_kurgan_meteodata$tavg<5, "tavg"] = 0
summary(years_kurgan_meteodata)
#Группируем по метеостанциям, годам и месяцам
alldays=group_by(years_kurgan_meteodata,id,year,month)
sumT_alldays_kurgan=summarize(alldays,tsum=sum(tavg))
summary(sumT_alldays_kurgan)
#Сгруппируем данные по месяцам
groups_kurgan_months=group_by(sumT_alldays_kurgan,month)
groups_kurgan_months
#Найдем для всех метеостанций и всех лет среднее по месяцам
sumT_months=summarize(groups_kurgan_months,St=mean(tsum))
sumT_months
#Рассчитать Fi по месяцам
sumT_months=mutate(sumT_months,Fi=afi+bfi*y*St)
#Рассчитаемть Yi
sumT_months=mutate (sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100-Ej)) )
#Расчитывать урожай как сумму по месяцам:
Yield = sum(sumT_months$Yi); Yield
#Для Кургана урожайность пшеницы в 2017 году составила 16.9245 ц/га, по данным, полученным на основе средней суммы активных температур за предыдущие 14 лет, с 5 ближайших метеостанций