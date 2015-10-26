dkorderedlevelsdec = function(x) {
  y = table(x)
  names(y)[order(y*-1)]
}

buildAccordionBootstrap2 = function(name, item, expanded=F) {
  inclass = ifelse(expanded, "in", "")
  div(
    div(class="accordion-heading", 
      HTML(paste('<a class="accordion-toggle" data-toggle="collapse" href="#collapse',name,'">',name,'</a>', sep=""))
    ),
    div(id=paste("collapse",name,sep=""), class=paste("accordion-body collapse", inclass),
      div(class="accordion-inner", item)
    )
  )
}


accordion = function(name, ...) {
  div(class="panel-group", id = name, role="tablist", ...)
}

accordionPanel = function(title, item, dataparent, expanded=F) {
  inclass = ifelse(expanded, "in", "")
  mydataparent = ifelse(missing(dataparent), "", paste0('data-parent="#', dataparent, '"'))
  myitemid = paste0("collapse", make.names(title))
  
  div(class="panel panel-default", 
    div(class="panel-heading", role="tab",
      h4(class="panel-title", 
        #a(datatoggle="collapse", dataparent=mydataparent, href=paste0("#", myitemid), title)
        HTML(paste('<a class="collapsed" data-toggle="collapse" ', mydataparent, ' href="#', myitemid, '">', title,'</a>', sep=""))
      )
    ), # panel-heading
    div(id = myitemid, class=paste("panel-collapse collapse", inclass), role="tabpanel",
      div(class="panel-body", item)
    ) # panel-collapse
  )
}

dkpval = function(myp) {
  if (myp < 0.001) return("p < 0.001")
  if (myp < 0.01) return("p < 0.01")
  if (myp < 0.05) return("p < 0.05")
  return(sprintf("p = %.2f", myp))
}

dkReplace = function(mysource, myreplaces) {
  for (x in names(myreplaces)) {
    mysource = gsub(x, myreplaces[x], mysource)
  }
  return(mysource)
}

# enableControl <- function(id,session) {
#   session$sendCustomMessage(type="jsCode",
#                             list(code= paste("$('#",id,"').prop('disabled',false)",sep="")))
# }

jsCodeHandler = function() {
  tags$head(tags$script(HTML('
        Shiny.addCustomMessageHandler("jsCode",
          function(message) {
            console.log(message)
            eval(message.code);
          }
        );
      ')))
}