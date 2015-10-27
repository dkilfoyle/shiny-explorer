require(knitr)
require(brew)
library(tabplot) #install_github("tabplot", username="mtennekes", subdir="pkg")
library(ggplot2)
library(readxl)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  
  observeEvent(input$importFile, {
    inFile <- input$importFile
    if(is.null(inFile))
      return(NULL)
    # hack for readxl no extension open issue #85
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    xlsfile = paste(inFile$datapath, ".xlsx", sep="")
    updateSelectInput(session, "excelsheets", choices=excel_sheets(xlsfile))
  })
  
  observeEvent(input$assignxls, {
    xlsfile = paste(input$importFile$datapath, ".xlsx", sep="")
    x=read_excel(xlsfile, input$excelsheets)
    
    # clean up 1 - remove NA cols
    numna = sum(is.na(colnames(x)))
    newnames = paste("NACOL", 1:numna, sep="")
    colnames(x)[is.na(colnames(x))]=newnames
    
    # clean up 2 - convert spaces to dots
    names(x) <- sub(" ", ".", names(x))
    
    assign(input$xlsdataframe,x, envir=.GlobalEnv)
    updateSelectInput(session, "dataset", "Dataframe:", choices = getDataFrames(), selected = getDataFrames()[1])
  })
  
  getSelectedDF <- reactive({
    eval(parse(text=input$dataset))
  })
  
  # when dataset changed populate the summaries and variable selection dropdowns
  observeEvent(input$dataset, {
    dfinfo = getdfinfo(input$dataset)
    
    # Update the field selects
    updateSelectInput(session, "numerics", choices=getNumerics(input$dataset))
    updateSelectInput(session, "factors", choices=getFactors(input$dataset))
    updateSelectInput(session, "dates", choices=getDates(input$dataset))
    updateSelectInput(session, "logicals", choices=dfinfo$logicals$name)
    
    # Populate the summary tab
    if (length(dfinfo$numerics$name)==0)
      output$numericInfo = renderText({"There are no numeric fields"})
    else
      output$numericInfo = renderTable(as.data.frame(dfinfo$numerics)[,-1],  sanitize.text.function = function(x) x)

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
    
    session$onFlushed(function() {
      session$sendCustomMessage(type="jsCode", list(code= paste("$('.sparkline').sparkline('html', {type: 'box', raw: true});")))
    })
    
  })
  
  observeEvent(input$deleteSelections, {
    # clear the selected fields
    dfinfo = getdfinfo(input$dataset)
    updateSelectInput(session, "numerics", choices=getNumerics(input$dataset))
    updateSelectInput(session, "factors", choices=getFactors(input$dataset))
    updateSelectInput(session, "dates", choices=getDates(input$dataset))
    updateSelectInput(session, "logicals", choices=dfinfo$logicals$name)
  })
  
  observeEvent(input$go, {
    # show the Analysis tab panel 
    updateTabsetPanel(session, "mainPanelTabset", selected="Analysis")  
  })
  
  getAnalysis = eventReactive(input$go, {
    
    # Load the selected variables and dataframe
    #TODO refactor the get selected vars code - need to fix selectizeInput captioning
    rmdsub = "Error: There is no report template for this combination of selected fields."
    numerics = as.vector(sapply(input$numerics, function(x) { strsplit(x, "/")[[1]][1] })) # ugly hack to strip field info from selectizeInput
    factors = as.vector(sapply(input$factors, function(x) { strsplit(x, "/")[[1]][1] })) 
    dates = as.vector(sapply(input$dates, function(x) { strsplit(x, "/")[[1]][1] })) 
    logicals = input$logicals
    dfstr = input$dataset
    
    if ((length(numerics) + length(factors) + length(dates) + length(logicals)) == 0)
      output$analysis = return("Select some fields first")
    
    progress = shiny::Progress$new(session, min=1, max=3)
    on.exit(progress$close())
    progress$set(message="Building report")
    
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
    progress$inc()
    brewout = capture.output(brew(text=rmdsub))
    
    updateAceEditor(session, "acermd", mode="markdown", value=paste(brewout, collapse="\n"))
    
    # EXPERIMENT: rmarkdown version
    # Outcome: no particular advantage, require extra files and highlighting js = not used
#     writeLines(brewout, con=file("temp.rmd"))
#     library(rmarkdown)
#     render("temp.rmd", html_fragment(toc=T))
#     myhtml = paste(readLines("temp.html"), collapse="\n")
    
    if (input$chkggtheme) {
      theme_set(theme_classic())
    }
    else
      theme_set(theme_gray())
    
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
    
    

    progress$inc()

    session$onFlushed(function() {
      updateTabsetPanel(session, "mainPanelTabset", selected="Analysis") 
    })

    return(myhtml)
    
  })

  output$analysis = renderText({
    getAnalysis()
  })
  
  output$mydt = renderDataTable({getSelectedDF()}, options=list(lengthMenu = c(5, 10, 25), pageLength = 10))
  
  output$mytabplot = renderPlot({
    if (input$limittabplot) {
      #TODO refactor the get selected vars code - need to fix selectizeInput captioning
      numerics = as.vector(sapply(input$numerics, function(x) { strsplit(x, "/")[[1]][1] })) # ugly hack to strip field info from selectizeInput
      factors = as.vector(sapply(input$factors, function(x) { strsplit(x, "/")[[1]][1] })) 
      dates = as.vector(sapply(input$dates, function(x) { strsplit(x, "/")[[1]][1] })) 
      logicals = input$logicals
      vars = unlist(c(numerics, factors, dates, logicals))
      if (length(vars) > 0)
        tableplot(getSelectedDF()[, unlist(c(numerics, factors, dates, logicals))])
    }
    else
      tableplot(getSelectedDF())
  })
  
  output$pivotTable = renderRpivotTable({
    rpivotTable(data=getSelectedDF()) #, onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }"))
  })

})