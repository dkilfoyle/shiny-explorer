library(shiny)
library(datasets)
require(knitr)
require(markdown)
require(brew)
library(rCharts)
library(gsubfn)
library(highr)

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
  
  observe({
    if (input$go != 0) {
      # show the Analysis tab panel
      updateTabsetPanel(session, "mainPanelTabset", selected="Analysis") 
    }
  })
  
  output$analysis = renderText({
    
    if (input$go == 0)
      return("Select some fields first")
    
    #    progress = Progress$new(session,min=1,max=2)
    #    on.exit(progress$close())
    #    progress$set(message="Knitting!",detail="bla bla bla")
    
    isolate({
      # wrap in isolate so that changing the selectboxes doesn't immediately trigger a renderText
      # instead wait until go button pressed
      rmdsub = "Error: There is no report template for this combination of selected fields."
      numerics = input$numerics 
      factors = input$factors
      dfstr = input$dataset
    })
    
    # could simplify following by using knit_expand and {{mydf}}${{numeric1}} etc in templates
    # but this makes the templates hard to read and write
    
    if ((length(numerics) == 0) & (length(factors)==1))
    {
      rmdsource = paste(readLines("templates/factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)       
      rmdsub = gsub("factor1", factors[1], rmdsub)
    }
    
    if ((length(numerics) == 0) & (length(factors)==2))
    {
      rmdsource = paste(readLines("templates/factor2.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("factor1", factors[1], rmdsub)
      rmdsub = gsub("factor2", factors[2], rmdsub)
    }
    
    if ((length(numerics) == 1) & (length(factors)==0))
    {
      rmdsource = paste(readLines("templates/numeric1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
    }
    
    if ((length(numerics) == 1) & (length(factors)==1))
    {
      rmdsource = paste(readLines("templates/numeric1factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)  
    }
    
    if ((length(numerics) == 1) & (length(factors)==2))
    {
      rmdsource = paste(readLines("templates/numeric1factor2.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)  
      rmdsub = gsub("factor2", factors[2], rmdsub) 
    }
    
    if ((length(numerics) == 2) & (length(factors)==0))
    {
      rmdsource = paste(readLines("templates/numeric2.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("numeric2", numerics[2], rmdsub)
    }
    
    if ((length(numerics) > 2) & (length(factors)==0))
    {
      rmdsource = paste(readLines("templates/numeric3.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numericlist", paste('c("', paste(numerics, collapse='","'), '")', sep=""), rmdsub)
    }
    
    if ((length(numerics) == 2) & (length(factors) == 1))
    {
      rmdsource = paste(readLines("templates/numeric2factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("numeric2", numerics[2], rmdsub)  
      rmdsub = gsub("factor1", factors[1], rmdsub)
    } 
    
    if ((length(numerics) > 2) & (length(factors)==1))
    {
      rmdsource = paste(readLines("templates/numeric3factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numericlist", paste('c("', paste(numerics, collapse='","'), '")', sep=""), rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)
    }
    
    # For debugging
#     rmdsource = paste(readLines("templates/numeric1.rmd"), collapse="\n")
#     data(iris)
#     library(knitr)
#     library(brew)
#     library(highr)
#     rmdsub = gsub("mydf", "iris", rmdsource)
#     rmdsub = gsub("numeric1", "Sepal.Length", rmdsub)
#     brewout = capture.output(brew(text=rmdsub))
#     myhtml = knit2html(text = brewout, stylesheet="", fragment.only = TRUE)  

#     library(XML)
#     x=htmlParse(myhtml)
#     library(highr)
#     x3 = getNodeSet(x, "//pre/code[@class='r']")
#     x4= xmlSApply(x3, function(myNode) { 
#       mytxt = xmlValue(myNode)
#       xmlValue(myNode) = ""
#       addChildren(myNode, xmlParseString(paste(hilight(mytxt,format="html"),collapse="\n")))
#       })
    
    brewout = capture.output(brew(text=rmdsub))
    
    #TODO: knitr uses highr?
    myhtml = paste(#paste(readLines("templates/navbar.rms"), collapse="\n"),
      try(knit2html(text = brewout, stylesheet="", fragment.only = TRUE)),
      "<script>
            //$('#analysis pre code').each(function(i, e) {hljs.highlightBlock(e)});
            $('#analysis h1').after('<div id=\"toc\"></div>')
            generateTOC($('#toc')[0], $('#analysis')[0]);
        </script>", 
      sep = '\n')
    
    gsubfn("<pre><code class=\"r\">(.*?)</code></pre>", 
           ~ paste("
              <pre><code class=\"r\">",
              str_replace_all(paste(hilight(str_replace_all(x, "&quot;", "\""), format="html"),collapse="\n"), "\n", "<br>"),
              "</code></pre>", sep=""),
           myhtml)
  })
  
  # will need to change this to renderChart (not2) on new version
  output$mydt = renderChart2({
    dt = dTable(getSelectedDF())
    return(dt)
  })
  
})