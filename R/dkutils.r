dkorderedlevelsdec = function(x) {
  y = table(x)
  names(y)[order(y*-1)]
}

buildAccordion = function(name, item, expanded=F) {
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