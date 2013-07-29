library(shiny)
library(datasets)
require(knitr)
require(markdown)
require(brew)
library(rCharts)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  
  # Return the requested dataset
  getSelectedDFName <- reactive({
    input$dataset
  })
  
  getSelectedDF <- reactive({
    eval(parse(text=getSelectedDFName()))
  })
  
  getDFInfo <- reactive({
    getdfinfo(input$dataset)
  })
  
  output$numericControls = renderUI({
    selectInput("numerics", "", choices=getDFInfo()$numerics$name, selected="", multiple=T)
  })
  
  output$factorControls = renderUI({
    selectInput("factors", "", choices=getDFInfo()$factors$name, selected="", multiple=T)
  })
  
  output$dateControls = renderUI({
    selectInput("dates", "", choices=getDFInfo()$dates$name, selected="", multiple=T)
  }) 
  
  output$logicalControls = renderUI({
    selectInput("logicals", "", choices=getDFInfo()$logicals$name, selected="", multiple=T)
  })
  
  # Populate the summary tab
  output$numericInfo = renderTable(getDFInfo()$numerics[,-1])
  output$factorInfo = renderTable(getDFInfo()$factors[,-1])
  output$dateInfo = renderTable(getDFInfo()$dates[,-1])
  output$logicalInfo = renderTable(getDFInfo()$logicals[,-1])  
  
  output$analysis = renderText({
    
    if (input$go == 0)
      return(NULL)
    
    # show the Analysis tab panel
    # updateTabsetPanel(session, "mainPanelTabset","Analysis") # doesn't work ? why
    
    #    progress = Progress$new(session,min=1,max=2)
    #    on.exit(progress$close())
    #    progress$set(message="Knitting!",detail="bla bla bla")
    
    isolate({
      
      rmdsub = "Error: There is no report template for this combination of selected fields."
      numerics = input$numerics 
      factors = input$factors
      dfstr = input$dataset
      
    })
    
    if ((length(numerics) == 0) & (length(factors)==1))
    {
      rmdsource = readLines("templates/factor1.rmd")
      rmdsource = paste(rmdsource, collapse="\n")   # concatenate to a single string
      rmdsub = gsub("mydf", dfstr, rmdsource)       # could do this with brew but this is simpler and cleaner in the rmd source
      rmdsub = gsub("factor1", factors[1], rmdsub)
    }
    
    if ((length(numerics) == 0) & (length(factors)==2))
    {
      rmdsource = readLines("templates/factor2.rmd")
      rmdsource = paste(rmdsource, collapse="\n") 
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("factor1", factors[1], rmdsub)
      rmdsub = gsub("factor2", factors[2], rmdsub)
    }
    
    if ((length(numerics) == 1) & (length(factors)==0))
    {
      rmdsource = readLines("templates/numeric1.rmd")
      rmdsource = paste(rmdsource, collapse="\n") 
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
    }
    
    if ((length(numerics) == 1) & (length(factors)==1))
    {
      rmdsource = readLines("templates/numeric1factor1.rmd")
      rmdsource = paste(rmdsource, collapse="\n") 
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)  
    }
    
    if ((length(numerics) == 1) & (length(factors)==2))
    {
      rmdsource = readLines("templates/numeric1factor2.rmd")
      rmdsource = paste(rmdsource, collapse="\n") 
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)  
      rmdsub = gsub("factor2", factors[2], rmdsub) 
    }
    
    if ((length(numerics) == 2) & (length(factors)==0))
    {
      rmdsource = readLines("templates/numeric2.rmd")
      rmdsource = paste(rmdsource, collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("numeric2", numerics[2], rmdsub)
    }
    
    
    if ((length(numerics) > 2) & (length(factors)==0))
    {
      rmdsource = readLines("templates/numeric3.rmd")
      rmdsource = paste(rmdsource, collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numericlist", paste('c("', paste(numerics, collapse='","'), '")', sep=""), rmdsub)
    }
    
    if ((length(numerics) == 2) & (length(factors) == 1))
    {
      rmdsource = readLines("templates/numeric2factor1.rmd")
      rmdsource = paste(rmdsource, collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("numeric2", numerics[2], rmdsub)  
      rmdsub = gsub("factor1", factors[1], rmdsub)
    } 
    
    if ((length(numerics) > 2) & (length(factors)==1))
    {
      rmdsource = readLines("templates/numeric3factor1.rmd")
      rmdsource = paste(rmdsource, collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numericlist", paste('c("', paste(numerics, collapse='","'), '")', sep=""), rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)
    }
    
    brewout = capture.output(brew(text=rmdsub))
    
    paste(#paste(readLines("templates/navbar.rms"), collapse="\n"),
      try(knit2html(text = brewout, stylesheet="", fragment.only = TRUE)),
      "<script>
            $('#analysis pre code').each(function(i, e) {hljs.highlightBlock(e)});
            generateTOC($('#toc')[0], $('#analysis')[0]);
        </script>", 
      sep = '\n')
    
    #TODO: knitr uses highr?
    
    
  })
  
  # will need to change this to renderChart (not2) on new version
  output$mydt = renderChart2({
    dt = dTable(getSelectedDF())
    return(dt)
  })
  
})