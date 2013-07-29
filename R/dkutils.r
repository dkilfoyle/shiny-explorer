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