library(httr)
library(jsonlite)
library(dygraphs)

data = fromJSON(glue::glue(
  'https://api.met.no/weatherapi/locationforecast/',
  '2.0/compact?lat=-4.88905&lon=29.6623'),flatten = TRUE)

weather_data = data$properties$timeseries

tidy_weather = weather_data %>% 
  rename(
    pressure = ends_with("pressure_at_sea_level"),
    air_temp = ends_with("air_temperature"),
    cloudines = ends_with("cloud_area_fraction"),
    relative_humidity = ends_with("relative_humidity"),
    wind_speed = ends_with("wind_speed"),
    hour_1_precipitation = ends_with("data.next_1_hours.details.precipitation_amount"),
    hour_6_symbol = ends_with("data.next_6_hours.summary.symbol_code"),
    hour_1_symbol = ends_with("data.next_1_hours.summary.symbol_code"),
    wind_direction = ends_with("data.instant.details.wind_from_direction"),
    hour_12_symbol = ends_with("data.next_12_hours.summary.symbol_code"),
    hour_6_precipitation = ends_with("data.next_6_hours.details.precipitation_amount")
  ) %>% 
  mutate(
    datetime = lubridate::as_datetime(time)
  )

get_weather_dygraph = function(){
  temp = xts::xts(tidy_weather$air_temp,tidy_weather$datetime)
  precipitation_hour6 = xts::xts(tidy_weather$hour_6_precipitation,tidy_weather$datetime)
  precipitation_hour1 = xts::xts(tidy_weather$hour_1_precipitation,tidy_weather$datetime)
  humidity = xts::xts(tidy_weather$relative_humidity,tidy_weather$datetime)
  wind_speed = xts::xts(tidy_weather$wind_speed,tidy_weather$datetime)
  
  series = cbind(temp,precipitation_hour1,precipitation_hour6,
                 humidity,wind_speed)
  
  dygraph(series,main = 
            "Weather Data: Kigoma Region") %>% 
    dySeries("temp",label = "Temperature",strokeWidth = 3) %>% 
    dySeries("precipitation_hour1",label = "Precipitation1",strokeWidth = 3,
             strokePattern = "dashed") %>% 
    dySeries("precipitation_hour6", label = "Precipitation6",strokeWidth = 3, 
             strokePattern = "dotted") %>% 
    dySeries("humidity", label = "Humidity", strokeWidth = 3)
  
}
