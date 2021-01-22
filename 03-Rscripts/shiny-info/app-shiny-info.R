# based on https://shiny.rstudio.com/tutorial/

# 1-build the app ####

library(shiny)
# every shiny app webpage (ui) is maintained by a computer (server)

ui <- fluidPage( # user interface
  # wellPanel( # groups things into a grey well
  # column(5, # to arrange elements in columns. width compulsory, offset optional
  sidebarLayout(
    sidebarPanel(
      tags$p("Presta atención, calla y deja al",
             tags$strong("maestro"),
             "trabajar"),
         sliderInput(inputId = "num", # 1 input object
              label = "choose a number",
              value = 25, min = 1, max = 100)
         ),
  textInput(inputId = "title", # 2 input object
            label = "write a title",
            value = "histogram of normal values")
  ),
  # fluidRow( # to arrange elements in rows
  #   column(4, offset = 8,
  mainPanel(
           plotOutput("hist"), # add a space to plot the output (e.g. "hist")
  verbatimTextOutput("stats"), # add a space to add text in the output (e.g. "stats")
  actionButton(inputId = "clicks", # add a action button 1
               label = "click me"),
  actionButton(inputId = "go", # add a action button 2
               label = "update")
  )
  # actionButton(inputId = "norm", label = "normal"),
  # actionButton(inputId = "unif", label = "uniform"),
  # plotOutput("hist")
)

server <- function(input, output, session) { # server interface
  data <- reactive({ # builds a reactive object by creating a reactive expression (e.g. data() )
    rnorm(input$num)
  })
  output$hist <- renderPlot({ # output object 1 + reactive function 1 (i.e. the render function that makes objects to display)
    hist(data())
    # hist(rnorm(input$num), # reactive value: input$x
    # main = isolate({input$title})) # isolate(): create an output that it's not reactive
  })
  output$stats <- renderPrint({ # output object 2 + reactive function 2
    summary(data())
    # summary(rnorm(input$num))
  })
  observeEvent(input$clicks, { # triggers code to run on the server. click and run the code I specify later (add one)
    print(as.numeric(input$clicks))
  })
  
  # data2 <- eventReactive(input$go, { # delay reactions. for that create a reactive expression
  #   rnorm(input$num)
  # }) 
  # output$hist <- renderPlot({ # the histogram depends on the reactive expression that only invalidates when someone clicks the button
  #   hist(data2())
  # })
  
  # rv <- reactiveValues(data = rnorm(100)) # creates a list of reactive values to manipulate programatically
  # observeEvent(input$norm, {rv$data <- rnorm(100)}) # overwrite the data every time you press the action button
  # observeEvent(input$unif, {rv$data <- runif(100)})
  # output$hist <- renderPlot({
  #   hist(rv$data)
  # })
  
}

shinyApp(ui, server) # put them together and create the sinyapp

# between {} you put all the r code you want

# 2-control reactions ####
## 1-create a basic chain with render()
## 2-extent the chain with reactive() (modularize reactions)
## 3-break the chain with isolate()
## 4-trigger an arbitrary code with observeEvent()
## 5-delay reactions with eventReactive()
## 6-create your own reactive values with reactiveValues()

# reactive(). 2 characteristics:
## 1-you call a reactive expression like a function (e.g. data())
## 2-reactive expressions cache their values (the expression
# will return its most recent value, unless it has become invalidated)

# isolate()

# observeEvent()

# eventReactive()

# reactiveValues()


# 3-customize appearance ####
# how to add static content (html)
# inside the user interface
# In R the functions call html for you
# When writing R, yo add content with tags funcions
# tags$h1() <- <h1></h1>
# names(tags) 110 elements
# each tag is a function: i.e. tags$h1
# to run the function tags$h1() 

# to add an image from a file, save the file in a
# subdirectory named www

# some tags functions come with a wrapper function,
# so you do not need to call tags. e.g. a(), h1(), p()


## 3.1-create a layout ####
# use layout functions to position elements within you app
# add html that divides the ui into a grid

# fluidRow() adds rows to the grid. Each new row
# goes below the previous rows

# column() adds columns within a row. Each new column
# goes to the left of the previous column
# specify the width and offset of each column out of 12
# offset: the number of units between the previous column and this one


## 3.2-assemble layers of panels ####
# panels to group multiple elements into a siongle unit with
# its own properties

# wellPanel(): groups things into a grey well

# tabPanel(): creates a stackable layer of elements
# each tab is like a small ui of its own
# use tabs to navigate between tabpanels (like having different pages/layers)

# navlistPanel(): combines tabs into a single panel
# use lists to navigate between tabs
# (the same but panels are on the left corner instead of being on the top)


## 3.3-use a prepackaged layout ####
# sidebarLayout(): use with sidebarPanel() and 
# mainPanel() to divide app into sections

# fixedPage(): creates a page that defaults to 
# a width of 724, 940, or 1170 pixels
# depending on browser window

# navbarPage(): combines tabs into a single page
# replaces fluidPage(). requires title

# navbarMenu(): combines tab links into a dropdown menu
# for navbarPage()

# dashboardPage(): comes in the shinydashboard package
# a package of layout functions


## 3.4-style with css ####
# to make visually pleasant web pages
# Cascading Style Sheets (CSS) are a framework for
# customizing the appearence of elements in a web page
# style a web page in 3 ways:
## 1-link to an external css file: set the theme argument of fluidPage() to the .css filename
## or place in a link in the app's header with to the file with tags$head() and tags$link()
## 2-write global css in header: write global css with tags$head() and tags$syle() and HTML()
## or save the css as a file in your app directory and include it with includeCSS(). this it doesn't need to be in www
## 3-write individual css in a tag's style attribute: set the style argument in shiny's tag functions
# match styling to: tag, class or id
# it ovewrites the previous that's why is cascading
# shiny uses Bootstrap 3 css framework (see getbootstrap.com)
# place .css files in the www folder of your app directory
# to learn more about css & html see www.codeacademy.com/tracks/web

