dkorderedlevelsdec = function(x) {
  y = table(x)
  names(y)[order(y*-1)]
}

buildAccordion = function(name, dataparent, item, expanded=F) {
  inclass = ifelse(expanded, "in", "")
  div(class="panel panel-default", 
    div(class="panel-heading", role="tab",
      h4(class="panel-title", 
        HTML(paste('<a data-toggle="collapse" data-parent="#', dataparent, '" href="#collapse',name,'">',name,'</a>', sep=""))
      )
    ),
    div(id=paste("collapse",name,sep=""), class=paste("panel-collapse collapse", inclass),
        div(class="panel-body", item)
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