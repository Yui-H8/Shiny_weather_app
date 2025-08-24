# server.R

# 必要なパッケージの読み込み
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)

# 天気予報データ取得の関数を含むファイルを読み込み
source("model_prediction.R")

# 天気に応じた色を返す関数
color_by_weather <- function(weather) {
  case_when(
    weather == "Clear" ~ "yellow",
    weather == "Clouds" ~ "gray",
    weather == "Rain" ~ "blue",
    weather == "Snow" ~ "white",
    weather == "Thunderstorm" ~ "purple",
    TRUE ~ "darkred"  # 未知の天気など
  )
}

# Shinyサーバー定義
shinyServer(function(input, output) {
  
  # 天気データの取得（アプリ起動時に一度取得）
  city_weather_df <- generate_city_weather_data()
  
  # 都市ごとの最新データを取得（地図初期描画用）
  cities_max_weather <- city_weather_df %>%
    group_by(CITY_ASCII) %>%
    filter(FORECASTDATETIME == min(FORECASTDATETIME)) %>%
    slice(1) %>%
    ungroup()
  
  # 地図の初期描画
  output$city_weather_map <- renderLeaflet({
    req(cities_max_weather)
    leaflet(cities_max_weather) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LNG,
        lat = ~LAT,
        radius = 8,
        color = ~color_by_weather(WEATHER),
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~LABEL
      )
  })
  
  # 都市選択時の地図更新
  observeEvent(input$city_dropdown, {
    if (input$city_dropdown == "All") {
      data_to_show <- cities_max_weather
    } else {
      data_to_show <- cities_max_weather %>%
        filter(CITY_ASCII == input$city_dropdown)
    }
    
    leafletProxy("city_weather_map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = data_to_show,
        lng = ~LNG,
        lat = ~LAT,
        radius = 10,
        color = ~color_by_weather(WEATHER),
        fillOpacity = 0.8,
        stroke = TRUE,
        popup = ~DETAILED_LABEL
      )
  })
  
  # 温度の折れ線グラフ
  output$temp_line <- renderPlot({
    if (input$city_dropdown == "All") return(NULL)
    
    selected_city_data <- city_weather_df %>%
      filter(CITY_ASCII == input$city_dropdown)
    
    max_temp <- ceiling(max(selected_city_data$TEMPERATURE, na.rm = TRUE) / 2.5) * 2.5
    
    ggplot(selected_city_data, aes(x = FORECASTDATETIME, y = TEMPERATURE)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "red") +
      geom_text(aes(label = round(TEMPERATURE, 1)), vjust = -1, size = 3) +
      scale_x_datetime(
        date_breaks = "12 hours",
        date_labels = "%m/%d %H:%M"
      ) +
      scale_y_continuous(
        breaks = seq(0, max_temp, by = 2.5)
      ) +
      labs(
        title = paste("Temperature Forecast -", input$city_dropdown),
        x = "Time (3-hour forecast)",
        y = "Temperature (°C)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$wind_area_humidity <- renderPlot({
    if (input$city_dropdown == "All") return(NULL)
    
    df <- city_weather_df
    
    selected_city_data <- df %>%
      filter(CITY_ASCII == input$city_dropdown) %>%
      arrange(FORECASTDATETIME)
    
    ggplot(selected_city_data, aes(x = FORECASTDATETIME)) +
      geom_area(aes(y = HUMIDITY), fill = "lightblue", alpha = 0.3) +  # 湿度: パステルカラーの塗りつぶし
      geom_line(aes(y = WIND_SPEED * 10), color = "darkblue", size = 1) +  # 風速: 濃い色の線
      scale_y_continuous(
        name = "Humidity (%)",
        sec.axis = sec_axis(~ . / 10, name = "Wind Speed (m/s)")
      ) +
      labs(
        title = paste("Humidity(Area) and Wind Speed(Line) - ", input$city_dropdown),
        x = "Time",
        y = "Humidity (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
})


