library(shinyBS)
library(shiny)
library(shinyWidgets)
library(mapdeck)
library(shinydashboard)
library(highcharter)
library(shinyjs)

# shinyUI(
#   
#   div(class = "navbar-default",
#       # Load Css
#       tags$head(includeCSS("www/styles.css")),
#       # Select custom slider
#       # https://divadnojnarg.github.io/blog/customsliderinput/
#       chooseSliderSkin("HTML5", color = "#112446")
#       # Start navbar page
#       , navbarPage("ANAC Data", id = "tabs",
#                    # # Landing page
#                    # tabPanel(title = "", value = "tab_home",
#                    #          uiOutput('landing_page')),
#                    # Map page
#                    tabPanel(title = "Mapa", value = "tab_mapa", 
#                             # Output map
#                             mapdeckOutput("map"),
#                             # Create the side panel  
#                             absolutePanel(id = "controls", class = "panel panel-default", 
#                                           fixed = TRUE, draggable = FALSE,
#                                           top = 80, right = 20, width = 350, height = 615,
#                                           pickerInput(
#                                             inputId = "mes",
#                                             label = "Escolha o mes",
#                                             choices = c("Janeiro" = 1,
#                                                         "Fevereiro" = 2,
#                                                         "Março" = 3,
#                                                         "Abril" = 4,
#                                                         "Maio" = 5,
#                                                         "Junho" = 6)
#                                             
#                                           ),
#                                           pickerInput(
#                                             inputId = "top",
#                                             label = "Escolha a quantidade",
#                                             choices = c(10, 20, 30, 40, 50)
#                                           )
#                                           )
#                    )
#       )
#       # # About page
#       # tabPanel(id = "about", title = "Sobre",
#       #          includeMarkdown("about.md")
#       #          
#       #          
#       # )
#       
#       
#   )
#   
# )





header <- dashboardHeader(
  title = "Tráfego Aéreo de Passageiros no Brasil",
  titleWidth = 400,
  disable = FALSE,
  tags$li(class="dropdown", uiOutput("dica")),
  tags$li(class="dropdown", tags$a(href="https://kauebraga.dev/", "Contato", target="_blank")),
  # tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/abhinav-agrawal-pmp%C2%AE-itil%C2%AE-5720309/" ,icon("linkedin"), "My Profile", target="_blank")),
  tags$li(class="dropdown",tags$a(href="https://github.com/kauebraga/app_anac", icon("github"), "Código", target="_blank"))
)

body <- dashboardBody(
  fluidRow(
    column(width = 8,
           box(width = NULL, 
               # height = "100%",
               solidHeader = TRUE,
               # title = "Mapa",
               mapdeckOutput("map", height = "calc(100vh - 230px)")
           ),
           infoBoxOutput("voos", width = 6),
           infoBoxOutput("passageiros", width = 6)
           
           
           
    ),
    column(width = 4,
           box(width = NULL, 
               status = "primary",
               collapsible = FALSE,
               pickerInput(inputId = "mes", 
                           label = "Mês: ", 
                           choices = list("2020" = c("Janeiro" = 1,
                                       "Fevereiro" = 2,
                                       "Março" = 3,
                                       "Abril" = 4,
                                       "Maio" = 5,
                                       "Junho" = 6))),
               pickerInput(
                 inputId = "top",
                 label = "Quantidade de rotas mais carregadas: ",
                 choices = c(10, 20, 30, 40, 50)
               )
               # conditionalPanel(condition = "input.map_arc_click, input.Clicked",
                                

           ),
           box(width = NULL, 
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               title = div(style = "width: 100%", uiOutput("title_graph"), actionButton(inputId = "reset_input", label = "Voltar")),
               # div(style="display:inline-block; float:left", id="title_graph", class="h3 shiny-html-output"),
               # HTML('<button id="reset_input" style = "display:inline-block; float:right" type="button" class="btn btn-default action-button">Reset inputs</button>'),
               uiOutput("graph"))
    )
  ),
  
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.setInputValue(variableName, null);
    });
    "),
  tags$head(tags$script(src="highchart_lang.js"))
)


dashboardPage(
  title = "Tráfego Aéreo de Passageiros no Brasil",
  header,
  dashboardSidebar(disable = T),
  body,
  skin = "black"
)