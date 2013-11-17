library(shiny)
library(datasets)
require(knitr)
require(brew)
#library(rCharts)

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
  
  observe({
    dfinfo = getdfinfo(input$dataset)
    
    # Update the field selects
    updateSelectInput(session, "numerics", "", choices=dfinfo$numerics$name, selected="")
    updateSelectInput(session, "factors", "", choices=dfinfo$factors$name, selected="")
    updateSelectInput(session, "dates", "", choices=dfinfo$dates$name, selected="")
    updateSelectInput(session, "logicals", "", choices=dfinfo$logicals$name, selected="")
    
    # Populate the summary tab
    output$numericInfo = renderTable(dfinfo$numerics[,-1])
    output$factorInfo = renderTable(dfinfo$factors[,-1])
    output$dateInfo = renderTable(dfinfo$dates[,-1])
    output$logicalInfo = renderTable(dfinfo$logicals[,-1])
  })
  
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
      logicals = input$logicals
      dates = input$dates
      dfstr = input$dataset
    })
    
    # could simplify following by using knit_expand and {{mydf}}${{numeric1}} etc in templates
    # but this makes the templates hard to read and write
    
    if ((length(numerics) == 0) & (length(factors)==1)) {
      rmdsource = paste(readLines("templates/factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)       
      rmdsub = gsub("factor1", factors[1], rmdsub)
    }
    
    if ((length(numerics) == 0) & (length(factors)==2)) {
      rmdsource = paste(readLines("templates/factor2.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("factor1", factors[1], rmdsub)
      rmdsub = gsub("factor2", factors[2], rmdsub)
    }
    
    if ((length(numerics) == 1) & (length(factors)==0)) {
      rmdsource = paste(readLines("templates/numeric1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
    }
    
    if ((length(numerics) == 1) & (length(factors)==1)) {
      rmdsource = paste(readLines("templates/numeric1factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)  
    }
    
    if ((length(numerics) == 1) & (length(factors)==2)) {
      rmdsource = paste(readLines("templates/numeric1factor2.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)  
      rmdsub = gsub("factor2", factors[2], rmdsub) 
    }
    
    if ((length(numerics) == 2) & (length(factors)==0)) {
      rmdsource = paste(readLines("templates/numeric2.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("numeric2", numerics[2], rmdsub)
    }
    
    if ((length(numerics) > 2) & (length(factors)==0)) {
      rmdsource = paste(readLines("templates/numeric3.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numericlist", paste('c("', paste(numerics, collapse='","'), '")', sep=""), rmdsub)
    }
    
    if ((length(numerics) == 2) & (length(factors) == 1)) {
      rmdsource = paste(readLines("templates/numeric2factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numeric1", numerics[1], rmdsub)
      rmdsub = gsub("numeric2", numerics[2], rmdsub)  
      rmdsub = gsub("factor1", factors[1], rmdsub)
    } 
    
    if ((length(numerics) > 2) & (length(factors)==1)) {
      rmdsource = paste(readLines("templates/numeric3factor1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("numericlist", paste('c("', paste(numerics, collapse='","'), '")', sep=""), rmdsub)
      rmdsub = gsub("factor1", factors[1], rmdsub)
    }
    
    if ((length(logicals) == 1) & (length(factors) > 0)) {
      rmdsource = paste(readLines("templates/logical1.rmd"), collapse="\n")
      rmdsub = gsub("mydf", dfstr, rmdsource)
      rmdsub = gsub("logical1", logicals[1], rmdsub)
      rmdsub = gsub("factorlist", paste('c("', paste(factors, collapse='","'), '")', sep=""), rmdsub)
    }
    
    brewout = capture.output(brew(text=rmdsub))
    
    updateAceEditor(session, "acermd", mode="markdown", value=paste(brewout, collapse="\n"))
    
    render_html() # this sets hooks to use highr
    myhtml = paste(#paste(readLines("templates/navbar.rms"), collapse="\n"),
      try(knit2html(text = brewout, stylesheet="", fragment.only = TRUE)),
      "<script>
            // javascript highlighting not needed if using render_html()
            // $('#analysis pre code').each(function(i, e) {hljs.highlightBlock(e)});
            
            // Insert a TOC after the h1 title
            $('#analysis h1').after('<div id=\"toc\"></div>')
            generateTOC($('#toc')[0], $('#analysis')[0]);
        </script>", 
      sep = '\n')
    
  })
  
  output$mydt = renderDataTable({getSelectedDF()}, options=list(aLengthMenu = c(5, 10, 25), iDisplayLength = 5))
  
})