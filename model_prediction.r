# model_prediction.R

require(tidyverse)
require(httr)

# OpenWeather API key
api_key <- "a52d7fe339c5cf40af6a567d8fccd250"  # ← 自分のキーを入れてください

# 天気予報を複数都市から取得
get_weather_forecast_by_cities <- function(city_names) {
  city <- c()
  weather <- c()
  temperature <- c()
  visibility <- c()
  humidity <- c()
  wind_speed <- c()
  seasons <- c()
  hours <- c()
  forecast_date <- c()
  weather_labels <- c()
  weather_details_labels <- c()
  
  for (city_name in city_names) {
    url_get <- "https://api.openweathermap.org/data/2.5/forecast"
    forecast_query <- list(q = city_name, appid = api_key, units = "metric")
    response <- GET(url_get, query = forecast_query)
    
    if (status_code(response) != 200) {
      warning(paste("Failed to fetch weather for", city_name))
      next
    }
    
    json_list <- content(response, as = "parsed")
    results <- json_list$list
    
    for (result in results) {
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      temperature <- c(temperature, result$main$temp)
      visibility <- c(visibility, result$visibility)
      humidity <- c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      
      forecast_datetime <- result$dt_txt
      hour <- as.numeric(strftime(forecast_datetime, format = "%H"))
      month <- as.numeric(strftime(forecast_datetime, format = "%m"))
      forecast_date <- c(forecast_date, forecast_datetime)
      
      season <- case_when(
        month %in% 3:5 ~ "SPRING",
        month %in% 6:8 ~ "SUMMER",
        month %in% 9:11 ~ "AUTUMN",
        TRUE ~ "WINTER"
      )
      
      seasons <- c(seasons, season)
      hours <- c(hours, hour)
      
      # 簡易HTMLラベル
      weather_label <- paste0(
        "<b>", city_name, "</b><br/>",
        "<b>", result$weather[[1]]$main, "</b><br/>"
      )
      
      weather_detail_label <- paste0(
        "<b>", city_name, "</b><br/>",
        "<b>", result$weather[[1]]$main, "</b><br/>",
        "Temperature: ", result$main$temp, " °C<br/>",
        "Visibility: ", result$visibility, " m<br/>",
        "Humidity: ", result$main$humidity, " %<br/>",
        "Wind Speed: ", result$wind$speed, " m/s<br/>",
        "Datetime: ", forecast_datetime, "<br/>"
      )
      
      weather_labels <- c(weather_labels, weather_label)
      weather_details_labels <- c(weather_details_labels, weather_detail_label)
    }
  }
  
  tibble(
    CITY_ASCII = city,
    WEATHER = weather,
    TEMPERATURE = temperature,
    VISIBILITY = visibility,
    HUMIDITY = humidity,
    WIND_SPEED = wind_speed,
    SEASONS = seasons,
    HOURS = hours,
    FORECASTDATETIME = as.POSIXct(forecast_date),
    LABEL = weather_labels,
    DETAILED_LABEL = weather_details_labels
  )
}


# 都市リストを使って天気データフレームを生成
generate_city_weather_data <- function() {
  cities_df <- read_csv("updated_cities.csv")  # ← 適切なCSVファイル名にしてください
  weather_df <- get_weather_forecast_by_cities(cities_df$CITY_ASCII)
  
  cities_weather <- cities_df %>%
    left_join(weather_df, by = "CITY_ASCII") %>%
    select(CITY_ASCII, LNG, LAT, TEMPERATURE, WIND_SPEED, HUMIDITY, WEATHER,
           LABEL, DETAILED_LABEL, FORECASTDATETIME)
  
  return(cities_weather)
}
