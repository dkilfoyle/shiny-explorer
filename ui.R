library(shiny)
library(shinyAce)
source("R/dkdfinfo.r")
source("R/dkgraph.r")
source("R/dkutils.r")

# to run
# shiny:::runApp("../shiny-explorer")
# shiny:::runApp("../shiny-explorer", aunch.browser = rstudio::viewer)

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
                 "and click Analyse to show an analysis."))
    ),
    
    div(class="accordion", id ="fieldsAccordion", 
        div(class="accordion-group", 
            buildAccordion("Numerics", selectInput("numerics", "", choices=getdfinfo(getDataFrames()[1])$numerics$name, selected="", multiple=T), expanded=T),
            buildAccordion("Factors",  selectInput("factors", "", choices=getdfinfo(getDataFrames()[1])$factors$name, selected="", multiple=T)),
            buildAccordion("Dates",    selectInput("dates", "", choices=getdfinfo(getDataFrames()[1])$dates$name, selected="", multiple=T)),  
            buildAccordion("Logicals", selectInput("logicals", "", choices=getdfinfo(getDataFrames()[1])$logicals$name, selected="", multiple=T))
        )
    ),
  
    # clear field selects via client side javascript only
    p(
      # use actionButton rather than submitButton so that changing the dataframe dropdown automatically updates the field selects
      actionButton("go",strong("Analyse")), 
      
      HTML("<button class=\"btn\" onclick=\"
            $('#numerics').select2('val',''); $('#factors').select2('val',''); $('#dates').select2('val',''); $('#logicals').select2('val','');
           \">Clear Selections</button>")
    ),
    
    p(div(id="kniting", class="alert alert-info", style="display: none", "Kniting report"))
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