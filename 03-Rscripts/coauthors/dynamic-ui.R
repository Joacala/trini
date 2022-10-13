# https://mastering-shiny.org/action-dynamic.html
library(tidyverse)

# a function that creates the UI
# for a single variable. It’ll return a range slider
# for numeric inputs
make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], 
                value = mean(rng))
  } else {
    NULL
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      strong("This is an interactive line"),
      actionButton(inputId = "reset", # para poder mover el umbral
                   label = "reset"),
    make_ui(USArrests$UrbanPop, "UrbanPop"),
      ),
    mainPanel(
      plotOutput("vline"))
  )
)


server <- function(input, output, session) {
  data <- eventReactive(input$reset, { # hasta que clickas el boton no se actualizara la linea
    USArrests$UrbanPop
  })
  observeEvent(data(), {
    updateSliderInput(session, "UrbanPop",
                      value = mean(USArrests$UrbanPop) )
  })
  output$vline <- renderPlot({ 
    ggplot(USArrests, aes(UrbanPop, Murder)) +
      geom_point() +
      geom_vline(aes(xintercept = mean(input$UrbanPop)))
  })
  
}

# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       map(names(iris), ~ make_ui(iris[[.x]], .x))
#     ),
#     mainPanel(
#       tableOutput("data")
#     )
#   )
# )


# server <- function(input, output, session) {
#   selected <- reactive({
#     each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
#     reduce(each_var, ~ .x & .y)
#   })
#   
#   output$data <- renderTable(head(iris[selected(), ], 12))
# }

shinyApp(ui, server)
