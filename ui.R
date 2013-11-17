library(shiny)
library(shinyAce)
source("R/dkdfinfo.r")
source("R/dkgraph.r")
source("R/dkutils.r")

# to run
# shiny:::runApp("../shiny-explorer")

data(iris)

# for testing logistic regression
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

# progressInit()

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel(""),
  
  sidebarPanel(
    
    h2("Shiny Explorer"),
    
    wellPanel(
      selectInput("dataset", "Dataframe:", choices = getDataFrames()),
      p(helpText("Choose the desired fields in the dropdowns",
                 "and select Update View to show an analysis."))
    ),
    
    div(class="accordion", id ="fieldsAccordion", 
        div(class="accordion-group", 
            buildAccordion("Numerics", uiOutput("numericControls"), expanded=T),
            buildAccordion("Factors",  uiOutput("factorControls")),
            buildAccordion("Dates",    uiOutput("dateControls")),  
            buildAccordion("Logicals", uiOutput("logicalControls"))
        )
    ),
  
    # clear field selects via client side javascript only
    p(
      actionButton("go","GO!"),
      HTML("<button class=\"btn\" onclick=\"$('#numerics option').each(function(i, e) { e.selected = false});\">Clear Selections</button>")
    ),
    
    # TODO: the panel doesn't get hidden once shiny-busy class is removed ?why
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     br(),
                     div(class="alert alert-info", "Kniting report"))
  ),

  mainPanel(
    
    includeHTML("www/js/tools.js"),
    
    tabsetPanel(id="mainPanelTabset",
      tabPanel("Summary", 
              h4("Numerics"),
              tableOutput("numericInfo"),
              h4("Factors"),
              tableOutput("factorInfo"),
              h4("Dates"),
              tableOutput("dateInfo"),
              h4("Logicals"),
              tableOutput("logicalInfo"),
              ## consider using tableplot from tabplot here,
              div(HTML("<hr>")),
              dataTableOutput("mydt")
      ),
      tabPanel("Analysis", 
               htmlOutput("analysis")
      ),
      tabPanel("Source",
               aceEditor("acermd", mode="markdown"))
    )
  )
))