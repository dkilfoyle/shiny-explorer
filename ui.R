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
#mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title.
  headerPanel(""),
  
  sidebarPanel(
    
    includeCSS("www/css/dkknitr.css"),
    
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
                           selectizeInput("numerics", "", choices=getdfinfo(getDataFrames()[1])$numerics$name, selected="", multiple=T,
                                       options=list(placeholder="Select numeric(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""))), expanded=T),
            buildAccordion("Factors",  
                           selectizeInput("factors", "", choices=getdfinfo(getDataFrames()[1])$factors$name, selected="", multiple=T,
                                       options=list(placeholder="Select factor(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop="")))),
            buildAccordion("Dates",    
                           selectizeInput("dates", "", choices=getdfinfo(getDataFrames()[1])$dates$name, selected="", multiple=T, 
                                       options=list(placeholder="Select date(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop="")))),
            buildAccordion("Logicals", 
                           selectizeInput("logicals", "", choices=getdfinfo(getDataFrames()[1])$logicals$name, selected="", multiple=T,
                                       options=list(placeholder="Select logical(s)", dropdownParent = "body", plugins=list(remove_button="", drag_drop=""))))
        )
    ),
  
    p(
      # use actionButton rather than submitButton so that changing the dataframe dropdown automatically updates the field selects
      actionButton("go",strong("Analyse")), 
      actionButton("deleteSelections", "Clear Selections")
    ),
    
    # custom Progress notification
    tagList(singleton(tags$head(tags$script(src="js/customProgress.js")))),
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