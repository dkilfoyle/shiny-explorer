library(shiny)
library(shinyAce)
source("R/dkdfinfo.r")
source("R/dkgraph.r")
source("R/dkutils.r")

# to run
# shiny:::runApp("../shiny-explorer")
# shiny:::runApp("../shiny-explorer", launch.browser = rstudio::viewer)

data(iris)
data(airquality)

# a very ugly hack as can't send more data than character vectors to selectizeInput therefore need to encode the field info in the label and extract it
# NB: also need to strip the field info when
selectizeRenderStr = "
  {
    item:function(item,escape) {
      return '<div><span class=\"selectize-numitem\">' + escape((item.value).split('/')[0]) + '</span></div>'; 
    },
    option:function(item,escape) {
      return '<div><span class=\"selectize-name\">' + escape((item.label).split('/')[0]) + '</span><span class=\"selectize-caption\">' + escape((item.label).split('/')[1]) + '</span></div>'; 
    }
  }"

# for testing logistic regression
#mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

# Define UI for dataset viewer application
shinyUI(navbarPage("Shiny-Explorer", 
  
  tabPanel("Explorer", icon=icon("list"),
  
  sidebarLayout(
    sidebarPanel(
      
      # header includes
      includeCSS("www/css/dkknitr.css"),
      includeScript("www/js/jquery-ui-1.10.3.custom.min.js"),
      includeScript("www/js/jquery.sparkline.min.js"),
      includeScript("www/js/highlight.pack.js"),
      includeScript("www/js/toc.js"),
      jsCodeHandler(), # for sending custom JS code to execute
      
      h3("Variable Selection"),
      
      wellPanel(
        selectInput("dataset", "Dataframe:", choices = getDataFrames()),
        p(helpText("Choose the desired fields in the dropdowns",
                   "and click Analyse to show an analysis."))
      ),
      
      accordion("fieldsAccordion", 
        accordionPanel("Numerics", 
           selectizeInput("numerics", label="", choices=NULL, selected="", multiple=T, #NB: choices is filled by observing input$dataset
                          options=list(placeholder="Select numeric(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""), 
                                       labelField="label", render = I(selectizeRenderStr))), expanded=T),
        accordionPanel("Factors", 
           selectizeInput("factors", label="", choices=NULL, selected="", multiple=T,
                          options=list(placeholder="Select factor(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""),
                                       labelField="label", render = I(selectizeRenderStr)))),
        accordionPanel("Dates", 
           selectizeInput("dates", label="", choices=NULL, selected="", multiple=T, 
                                   options=list(placeholder="Select date(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""),
                                                labelField="label", render = I(selectizeRenderStr)))),
        accordionPanel("Logicals", 
           selectizeInput("logicals", label="", choices=NULL, selected="", multiple=T,
                     options=list(placeholder="Select logical(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop="") )))
      ),
    
      p(
        # use actionButton rather than submitButton so that changing the dataframe dropdown automatically updates the field selects
        actionButton("go",strong("Analyse"), icon("play")), 
        actionButton("deleteSelections", "Clear Selections", icon("trash-o"))
      )
    
    ), # sidebarPanel

    mainPanel(
      
      tabsetPanel(id="mainPanelTabset",
        tabPanel("Summary", 
                 
           # TODO: ? can do dropdown tabpanels instead of nested
           tabsetPanel(id="summaryTabset",
             tabPanel("Variables",               
                h4("Numerics"),
                tableOutput("numericInfo"),
                h4("Factors"),
                tableOutput("factorInfo"),
                h4("Dates"),
                tableOutput("dateInfo"),
                h4("Logicals"),
                tableOutput("logicalInfo")
                # ,plotOutput("tabplot")
             ),
             tabPanel("TabPlot",
              checkboxInput("limittabplot", label="Show selected variables only"),
              plotOutput("mytabplot")
             )
           )
        ),
        tabPanel("Table", 
                 dataTableOutput("mydt")
        ),
        tabPanel("Analysis", 
                 htmlOutput("analysis")
        ),
        tabPanel("Source",
                 aceEditor("acermd", mode="markdown"))
      )
    ) # mainPanel
  
  ) # sidebarLayout
  ), # tabPanel(Explorer)

  navbarMenu("Tests", icon=icon("bar-chart"),
     tabPanel("2 Sample"),
     tabPanel("Correlation")
  ) # navbarMenu(Tests)
  
))