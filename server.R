library(dplyr)
library(data.table)
library(shiny)
library(shinyWidgets)
library(mapdeck)
library(shinyBS)
library(ggplot2)
library(highcharter)
library(shinyjs)
library(lubridate)

set_token(fread("data-raw/mapbox_key.txt")$V1)

# set options for portuguese
lang <- getOption("highcharter.lang")
lang$decimalPoint <- ","
lang$months <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')
lang$shortMonths <- c('Jan', 'Fev', 'Mar', 'Abr', 'Maio', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez')
lang$weekdays <- c('Domingo', 'Segunda', 'Terça', 'Quarta', 'Quinta', 'Sexta', 'Sábado')
# lang$numericSymbols <- highcharter::JS("null") # optional: remove the SI prefixes
options(highcharter.lang = lang)

# open data
data <- fread("data/air_odmatrix_month_new.csv") %>%
  mutate(od = paste0(name_muni_uf_from, " ",
                     icon("arrow-right"), " ",
                     name_muni_uf_to)) %>%
  mutate(label_y = paste0(sg_iata_origem, " ", icon("plane"), " ", sg_iata_destino, "</i>"))

od1 <- fread("data/air_odmatrix.csv")


server <- function(input, output, session) {
  
  # filter month
  mes_filtrado <- reactive({
    
    data[month == input$mes]
    
  })
  
  
  # filter top
  top_filtrado <- reactive({
    
    mes_filtrado() %>%
      arrange(desc(total_pass)) %>%
      slice(1:input$top) %>%  
      # calculate stroke
      mutate(stroke = scales::rescale(total_pass, to = c(3, 15))) %>%
      # make tooltip
      mutate(rank = 1:n()) %>%
      mutate(tooltip = sprintf('<p><strong>%s</strong><br><strong>Passageiros/mês:</strong> %s<br><strong>Rank:</strong> %s</p>',
                               od, total_pass, rank))
    
  })
  
  
  
  
  # RENDER GRAPH ----------------------------------------------------------------------------------
  
  
  # # get map click ------------------------------------
  # observeEvent({input$map_arc_click},{
  #   
  #   # print( input$map_polygon_click )
  #   
  #   js <- input$map_arc_click
  #   lst <- jsonlite::fromJSON( js )
  #   row <- (lst$index) + 1
  #   
  #   # print(lst$index)
  #   # print(row)
  #   
  #   que <- top_filtrado() %>% mutate(label_y = paste0(sg_iata_origem, sg_iata_destino)) %>% filter(rank == row)
  #   
  #   print(que$total_pass)
  #   print(que$label_y)
  #   
  # })
  
  
  v <- reactiveValues()
  
  observeEvent(c(input$map_arc_click, input$Clicked), {
  # observeEvent(c(input$mes, input$top), {
    
    # session$sendCustomMessage(type = "resetValue", message = "map_arc_click, Clicked")
    # session$sendCustomMessage(type = "resetValue", message = "Clicked")
    
    shinyjs::enable("reset_input")
    
  })
  
  observeEvent(c(input$mes, input$top, input$reset_input), {
    
    shinyjs::disable("reset_input")
    
    v$data <- top_filtrado()
    
    
    # https://stackoverflow.com/questions/37208989/how-to-know-information-about-the-clicked-bar-in-highchart-column-r-shiny-plot/37212681#37212681
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.category);}")
    alt_click <- JS("function(event) {Shiny.onInputChange('Clicked', 'Alt: ' + event.altKey);}")
    
    v$plot <- hchart(v$data, "bar", hcaes(y = total_pass, x = label_y),
           name = "Frequência") %>%
      # hc_title(text = title_plot,
      #          align = "left", x = 50) %>%
      # hc_subtitle(text = legend_plot,
      #             align = "left", x = 50) %>%
      hc_xAxis(opposite = FALSE,
               title = list(text = "")
               , labels = list(
                 useHTML = TRUE,
                 # format = "{value}%",
                 style = list(fontSize = 15))
      ) %>%
      hc_yAxis(opposite = FALSE,
               title = list(text = "Passageiros")
               , labels = list(style = list(fontSize = 15))
      ) %>%
      # hc_yAxis(title = list(text = ifelse(input$graph_type == "palma_renda",
      #                                     i18n()$t("Razão de Palma"),
      #                                     i18n()$t("Razão de Desigualdade por Cor")))) %>%
      # change bar colors
      # hc_colors(colors = "#F4F4F4") %>%
      # change font
      hc_chart(style = list(fontFamily = "Roboto Condensed")) %>%
      # # add vertical line
      # hc_yAxis(plotLines = list(list(color = "#99A3A4", value = 1, width = 2, zIndex = 5, dashStyle = "LongDash"))) %>%
      # hc_exporting(enabled = FALSE) %>%
      # add data label at the end of each bar (with values)
      hc_plotOptions(bar = list(borderRadius = 1,
                                borderColor = "#000000",
                                color = "#F4F4F4",
                                tooltip = list(
                                  pointFormat = sprintf("%s: {point.y}", "Passageiros"),
                                  valueDecimals = 0),
                                # stacking = FALSE,
                                events = list(click = ClickFunction),
                                allowPointSelect = TRUE
                                
                                
      ))
    
    v$panel_title <- "Rotas mais carregadas"
    
    v$type <- TRUE
    
    
    
    # session$sendCustomMessage(type = "resetValue", message = "map_arc_click")
    # session$sendCustomMessage(type = "resetValue", message = "Clicked")
    

    
  })
  
  
  
  
  
  # observeEvent(input$reset_input, {
  #   
  #   mapdeck_update(map_id = "map") %>%
  #     mapdeck_view(location = c(-43.95988, -19.902739), zoom = 3, transition = "fly",
  #                  duration = "2000") %>%
  #     add_arc(
  #       data = top_filtrado()
  #       , layer_id = "arc_layer"
  #       , origin = c("lon_from", "lat_from")
  #       , destination = c("lon_to", "lat_to")
  #       # , stroke_from = "airport1"
  #       # , stroke_to = "airport2"
  #       , stroke_to_opacity = 50
  #       , stroke_width = "stroke"
  #       , update_view = FALSE
  #       , focus_layer = FALSE
  #       , tooltip = "tooltip"
  #       , auto_highlight = TRUE
  #       , tilt = 3
  #     )
  #   
  # })
  
  
  # get input (either from map or graph) that will filter graph
  observeEvent(c(input$map_arc_click), {v$input_graph_change <- row_map()})
  observeEvent(c(input$Clicked), {v$input_graph_change <- row_graph()})
  
  observeEvent(c(input$map_arc_click, input$Clicked), {
    
    print(paste0("teste: ", v$input_graph_change))
    
    data <- v$data %>% filter(rank == v$input_graph_change)
    
    data_od <- od1 %>%
      filter(sg_iata_origem == unique(data$sg_iata_origem), sg_iata_destino == unique(data$sg_iata_destino)) %>%
      arrange(date)
    
    month_numeric <- input$mes
    
    month_start <- as.Date(paste0("2020-", paste0(0, month_numeric, "-", "01"), tz = "UTC"))
    month_end <- as.Date(paste0("2020-", paste0(0, month_numeric, "-", ifelse(month_numeric == 2, "28", "30")), tz = "UTC"))
    month_string <- lubridate::month(month_start, label = TRUE)
    
    # v$plot <- hchart(data, "bar", hcaes(y = total_pass, x = label_y),
    #                  name = "Frequência") %>%
    #   hc_xAxis(opposite = FALSE,
    #            title = list(text = "")
    #            , labels = list(
    #              useHTML = TRUE,
    #              # format = "{value}%",
    #              style = list(fontSize = 15))
    #   ) %>%
    #   hc_yAxis(opposite = FALSE,
    #            title = list(text = "Passageiros")
    #            , labels = list(style = list(fontSize = 15))
    #   ) %>%
    #   hc_plotOptions(bar = list(borderRadius = 1,
    #                             borderColor = "#000000",
    #                             color = "#F4F4F4",
    #                             tooltip = list(
    #                               pointFormat = sprintf("%s: {point.y}", "Passageiros"),
    #                               valueDecimals = 0)
    #                             # stacking = FALSE,
    #                             
    #                             
    #   ))
    
    v$plot <- hchart(data_od, "line", hcaes(y = total_pass, x = date),
                     name = "Frequência") %>%
      hc_yAxis(labels = list(enabled = FALSE),
               title = list(text = ""),
               tickLength = 0,
               gridLineWidth = 0) %>%
      hc_plotOptions(
        line = list(color = "#000", lineWidth = 1,
                    tooltip = list(
                      pointFormat = sprintf("%s: {point.y}", "Passageiros"),
                      valueDecimals = 0))) %>%
      hc_xAxis(
        title = list(text = ""),
        labels = list(enabled = TRUE),
        # ticks every month
        tickInterval =  1000 * 3600 * 24 *30,
        plotBands = list(
          list(
            label = list(text = month_string),
            color = "rgba(20, 0, 0, 0.1)",
            from = datetime_to_timestamp(month_start),
            to = datetime_to_timestamp(month_end)
          )),
        dateTimeLabelFormats = list(month = '%b')) %>%
      hc_title(text = "<b>Distribuição diária dos passageiros em 2020</b>",
               align = "left",
               style = list(fontSize = "18px", fontFamily = "Roboto Condensed", useHTML = TRUE)) %>%
      hc_chart(style = list(fontFamily = "Roboto Condensed")) %>%
      hc_tooltip(xDateFormat = '%d-%m-%Y',
                 shared = TRUE)
    
    v$type <- FALSE
    
    v$panel_title <- data$label_y
    
    # shinyjs::reset("map_arc_click")
    
    
    # reset input to NULL
    # session$sendCustomMessage(type = "resetValue", message = "map_arc_click")
    
    
  })
  
  output$graph_all <- renderHighchart({
    
    
    v$plot
    
    
    
  })
  
  
  output$title_graph = renderText({
    
    v$panel_title
    
  })  
  
  

# RENDER UI -----------------------------------------------------------------------------------

  output$graph <- renderUI({
    
    
    if (!isTRUE(v$type)) {
      
      div(infoBoxOutput("od", width = NULL),
          infoBoxOutput("tempo_viagem", width = NULL),
          infoBoxOutput("aeronave", width = NULL),
          infoBoxOutput("distance", width = NULL),
          highchartOutput("graph_all", height = "225px"))
      
    } else if (isTRUE(v$type)) {
      
      div(highchartOutput("graph_all", height = "calc(100vh - 340px)"))
  
    }
    
  })  
  
  # reset graphics ------------------------------------------------------------------------------

  
  

  
    
  
  
  observeEvent(input$Clicked, {
    
    print(paste0("You clicked into the legend and selected series ", input$Clicked, "."))
    # print(paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".") )
  })
  
  
  # RENDER BRAZIL'S BASEMAP -------------------------------------------------------
  
  output$map <- renderMapdeck({
    
    mapdeck(location = c(-43.95988, -19.902739), zoom = 3)
    
    
    
  })
  
  
  observeEvent({c(input$mes, input$top)},{
    
    mapdeck_update(map_id = "map") %>%
      add_arc(
        data = top_filtrado()
        , layer_id = "arc_layer"
        , origin = c("lon_from", "lat_from")
        , destination = c("lon_to", "lat_to")
        # , stroke_from = "airport1"
        # , stroke_to = "airport2"
        , stroke_to_opacity = 50
        , stroke_width = "stroke"
        , update_view = FALSE
        , focus_layer = FALSE
        , tooltip = "tooltip"
        , auto_highlight = TRUE
        , tilt = 3
      )
    
   
    
    
  })
  
  
  
  
  row_map <- reactive({
    
    req(input$map_arc_click)
    
    js <- input$map_arc_click
    lst <- jsonlite::fromJSON( js )
    row <- (lst$index) + 1
    
    
    
  })
  
  
  row_graph <- reactive({
    
    req(input$Clicked)
    
    input$Clicked + 1
    
    
    
  })
  
  
  
  # observeEvent({input$map_arc_click},{
  #   
  #   row <- row_map()
  #   
  #   view <- top_filtrado() %>% filter(rank == row)
  #   
  #   mapdeck_update(map_id = "map") %>%
  #     mapdeck_view(location = c(view$lon_from, view$lat_from), zoom = 7, transition = "fly",
  #                  duration = "2000") %>%
  #     add_arc(
  #       data = view
  #       , layer_id = "arc_layer"
  #       , origin = c("lon_from", "lat_from")
  #       , destination = c("lon_to", "lat_to")
  #       # , stroke_from = "airport1"
  #       # , stroke_to = "airport2"
  #       , stroke_to_opacity = 50
  #       , stroke_width = "stroke"
  #       , update_view = FALSE
  #       , focus_layer = FALSE
  #       , tooltip = "tooltip"
  #       , auto_highlight = TRUE
  #       , tilt = 3
  #     )
  #   
  #   
  # })
  
  
  
  # infobox --------------------------------------
  
  # extract infos from selected route
  
  route_df <- reactive({
    
    filter(top_filtrado(), rank == v$input_graph_change)

    
                                  
    
  })
  
  
  # popover
  output$dica <- renderUI({
    tags$span(
      popify(trigger = "click", placement = "left", bsButton("pointlessButton", "Dicas para uso", style = "primary", size = "default"),
             "Dicas para uso",
             "- Clique na rota no mapa ou na barra do gráfico para mais informações sobre a rota <br> - Segure a tecla <b>CTRL</b> enquanto passeia pelo mapa")
    )
  })
  
  
  output$od <- renderInfoBox({
    infoBox(
      title = "ROTA", 
      fill = FALSE,
      value =  HTML(route_df()$od),
      icon = icon("map-marker-alt"),
      color = "black"
    )
  })
  
  output$tempo_viagem <- renderInfoBox({
    infoBox(
      title = "Tempo de viagem", 
      fill = FALSE,
      value =  route_df()$travel_time,
      icon = icon("clock"),
      color = "black"
    )
  })
  
  output$aeronave <- renderInfoBox({
    infoBox(
      title = "Aeronave mais usada", 
      value =  route_df()$top_plane,
      icon = icon("plane"),
      color = "black"
    )
  })
  
  output$distance <- renderInfoBox({
    infoBox(
      title = "Distância", 
      value =  paste0(round(route_df()$dist_pair), " KM"),
      icon = icon("ruler-horizontal"),
      color = "black"
    )
  })
  
  output$passageiros <- renderInfoBox({
    infoBox(
      title = "Passageiros", 
      value =  ifelse(nchar(as.character(sum(mes_filtrado()$total_pass))) >= 7, 
                      paste0("Total de ", round(sum(mes_filtrado()$total_pass)/1000000, 2), " milhões de passageiros no mês"),
                      paste("Total de ", round(sum(mes_filtrado()$total_pass)/1000), " mil passageiros no mês")),
      
      icon = icon("users"),
      color = "black"
    )
  })
  
  output$voos <- renderInfoBox({
    infoBox(
      title = "Vôos", 
      value = paste0("Total de ", round(sum(mes_filtrado()$n_flights)/1000), " mil vôos no mês"), 
      icon = icon("plane"),
      color = "black"
    )
  })
  
  
}