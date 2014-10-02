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
    updateSelectInput(session, "numerics", choices=dfinfo$numerics$name)
    updateSelectInput(session, "factors", choices=dfinfo$factors$name)
    updateSelectInput(session, "dates", choices=dfinfo$dates$name)
    updateSelectInput(session, "logicals", choices=dfinfo$logicals$name)
    
    # Populate the summary tab
    if (length(dfinfo$numerics$name)==0)
      output$numericInfo = renderText({"There are no numeric fields"})
    else
      output$numericInfo = renderTable(as.data.frame(dfinfo$numerics)[,-1])
    
    if (length(dfinfo$factors$name)==0)
      output$factorInfo = renderText({"There are no factor fields"})
    else
      output$factorInfo = renderTable(as.data.frame(dfinfo$factors)[,-1])
    
    if (length(dfinfo$dates$name)==0)
      output$dateInfo = renderText({"There are no date fields"})
    else
      output$dateInfo = renderTable(as.data.frame(dfinfo$dates)[,-1])
    
    if (length(dfinfo$logicals$name)==0)
      output$logicalInfo = renderText({"There are no logical fields"})
    else
      output$logicalInfo = renderTable(as.data.frame(dfinfo$logicals)[,-1])
  })
  
  observe({
    if (input$go != 0) {
      # show the Analysis tab panel
      updateTabsetPanel(session, "mainPanelTabset", selected="Analysis") 
    }
  })
  
  observe({
    if (input$deleteSelections != 0) {
      # clear the selected fields
      dfinfo = getdfinfo(input$dataset)
      updateSelectInput(session, "numerics", choices=dfinfo$numerics$name)
      updateSelectInput(session, "factors", choices=dfinfo$factors$name)
      updateSelectInput(session, "dates", choices=dfinfo$dates$name)
      updateSelectInput(session, "logicals", choices=dfinfo$logicals$name)
    }
  })
  
  output$analysis = renderText({
    
    if (input$go == 0)
      return("Select some fields first")
    
    #    progress = Progress$new(session,min=1,max=2)
    #    on.exit(progress$close())
    #    progress$set(message="Knitting!",detail="bla bla bla")
    
    session$sendCustomMessage(type="showmsg", list(mymsg="Hello"))
    
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
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, factor1=factors[1]))
    }
    
    if ((length(numerics) == 0) & (length(factors)==2)) {
      rmdsource = paste(readLines("templates/factor2.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, factor1=factors[1], factor2=factors[2]))
    }
    
    if ((length(numerics) == 1) & (length(factors)==0)) {
      rmdsource = paste(readLines("templates/numeric1.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, numeric1=numerics[1]))
    }
    
    if ((length(numerics) == 1) & (length(factors)==1)) {
      rmdsource = paste(readLines("templates/numeric1factor1.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, numeric1=numerics[1], factor1=factors[1]))
    }
    
    if ((length(numerics) == 1) & (length(factors)==2)) {
      rmdsource = paste(readLines("templates/numeric1factor2.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, numeric1=numerics[1], factor1=factors[1], factor2=factors[2]))
    }
    
    if ((length(numerics) == 2) & (length(factors)==0)) {
      rmdsource = paste(readLines("templates/numeric2.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, numeric1=numerics[1], numeric2=numerics[2]))
    }
    
    if ((length(numerics) > 2) & (length(factors)==0)) {
      rmdsource = paste(readLines("templates/numeric3.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, numericlist=paste('c("', paste(numerics, collapse='","'), '")', sep=""))) 
    }
    
    if ((length(numerics) == 2) & (length(factors) == 1)) {
      rmdsource = paste(readLines("templates/numeric2factor1.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, numeric1=numerics[1], numeric2=numerics[2], factor1=factors[1]))
    } 
    
    if ((length(numerics) > 2) & (length(factors)==1)) {
      rmdsource = paste(readLines("templates/numeric3factor1.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, numericlist=paste('c("', paste(numerics, collapse='","'), '")', sep=""), factor1=factors[1])) 
    }
    
    if ((length(logicals) == 1) & (length(factors) > 0)) {
      rmdsource = paste(readLines("templates/logical1.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, logical1=logicals[1], factorlist=paste('c("', paste(factors, collapse='","'), '")', sep="")))
    }
    
    # Some templates have conditional segements - hence use brew 
    # TODO: explore using whiskers instead of dkReplace/brew
    brewout = capture.output(brew(text=rmdsub))
    
    updateAceEditor(session, "acermd", mode="markdown", value=paste(brewout, collapse="\n"))
    
    # EXPERIMENT: rmarkdown version
    # Outcome: no particular advantage, require extra files and highlighting js = not used
#     writeLines(brewout, con=file("temp.rmd"))
#     library(rmarkdown)
#     render("temp.rmd", html_fragment(toc=T))
#     myhtml = paste(readLines("temp.html"), collapse="\n")
    
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
    
    session$sendCustomMessage(type="hidemsg", list(mymsg="Hello"))
    
    return(myhtml)
    
  })
  
  output$mydt = renderDataTable({getSelectedDF()}, options=list(lengthMenu = c(5, 10, 25), pageLength = 5))
  
})