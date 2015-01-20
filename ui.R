library(shiny)
library(shinyAce)
source("R/dkdfinfo.r")
source("R/dkgraph.r")
source("R/dkutils.r")

# to run
# shiny:::runApp("../shiny-explorer")
# shiny:::runApp("../shiny-explorer", aunch.browser = rstudio::viewer)

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
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel(""),
  
  sidebarPanel(
    
    includeCSS("www/css/dkknitr.css"),
    includeScript("www/js/jquery.sparkline.min.js"),
    jsCodeHandler(), # for sending custom JS code to execute
    
    h2("Shiny Explorer"),
    
    wellPanel(
      selectInput("dataset", "Dataframe:", choices = getDataFrames()),
      p(helpText("Choose the desired fields in the dropdowns",
                 "and click Analyse to show an analysis."))
    ),
    
    # jqueryui needed for selectize plugins
    tagList(singleton(tags$head(tags$script(src="js/jquery-ui-1.10.3.custom.min.js")))),
    
    div(class="accordion", id ="fieldsAccordion", 
        div(class="accordion-group", 
            buildAccordion("Numerics", 
                           selectizeInput("numerics", label="", choices=NULL, selected="", multiple=T, #NB: choices is filled by observing input$dataset
                                          options=list(placeholder="Select numeric(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""), 
                                                       labelField="label", render = I(selectizeRenderStr))), expanded=T),
            buildAccordion("Factors",  
                           selectizeInput("factors", label="", choices=NULL, selected="", multiple=T,
                                       options=list(placeholder="Select factor(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""),
                                                    labelField="label", render = I(selectizeRenderStr)))),
            buildAccordion("Dates",    
                           selectizeInput("dates", label="", choices=NULL, selected="", multiple=T, 
                                       options=list(placeholder="Select date(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""),
                                                    labelField="label", render = I(selectizeRenderStr)))),
            buildAccordion("Logicals", 
                           selectizeInput("logicals", label="", choices=NULL, selected="", multiple=T,
                                       options=list(placeholder="Select logical(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop="") )))
        )
    ),
  
    p(
      # use actionButton rather than submitButton so that changing the dataframe dropdown automatically updates the field selects
      actionButton("go",strong("Analyse")), 
      actionButton("deleteSelections", "Clear Selections")
    )
    

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