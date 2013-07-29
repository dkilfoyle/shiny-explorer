getDataFrames = function()
{
  ls(envir=.GlobalEnv)[sapply(ls(envir=.GlobalEnv), function(x){is.data.frame(get(x))})]
}

getdfinfo = function(dfn)
{
  mydf = get(dfn)
  fields = names(mydf)
  fields.type = sapply(fields, getVectorType, mydf)
  fields.numeric = fields[fields.type %in% c("numeric","integer","double", "logical")]
  fields.factor = fields[fields.type %in% c("factor", "binaryfactor")]
  fields.logical = fields[fields.type %in% c("logical","binaryfactor")]
  fields.date = fields[fields.type %in% c("date")]
  
  getdfinfo = list(
    numerics = data.frame(name=fields.numeric,
      mean=sapply(fields.numeric, function(x) { round(mean(mydf[,x], na.rm=T),2) } ),
      min =sapply(fields.numeric, function(x) { min(mydf[,x], na.rm=T)}),
      max =sapply(fields.numeric, function(x) { max(mydf[,x], na.rm=T) }),
      NAs =sapply(fields.numeric, function(x) { sum(is.na(mydf[,x])) } )),
    factors = data.frame(name=fields.factor,
      nlevels = sapply(fields.factor, function(x) { nlevels(mydf[,x]) }),
      NAs = sapply(fields.factor, function(x) { sum(is.na(mydf[,x])) })),
    logicals = data.frame(name=fields.logical,
      mean = sapply(fields.logical, function(x) {
        xx = mydf[,x]
        if (is.factor(xx)) {
          levels(xx) = c(0,1)
          xx=as.numeric(xx)
        }
        mean(xx, na.rm=T)    
      })),
    dates = data.frame(name=fields.date,
      min = sapply(fields.date, function(x) { min(mydf[,x], na.rm=T) }),
      max = sapply(fields.date, function(x) { max(mydf[x], na.rm=T) }),
      NAs = sapply(fields.date, function(x) { sum(is.na(mydf[,x])) }))
  )
}

getVectorType = function(field, mydf)
{
  x = "Unknown"
  if (is.double(mydf[,field])) { x = "double" }
  if (is.character(mydf[,field])) { x = "character" }
  if (is.integer(mydf[,field])) {
    x = "integer"
    if (sum(is.na(mydf[,field])) == 0 && max(mydf[,field]) == 1 && min(mydf[,field]) == 0) {
      x="logical"
    }
  }
  if (is.factor(mydf[,field])) { 
    x = "factor" 
    if (nlevels(mydf[,field]) == 2)
    {
      x="binaryfactor"
    }
  }
  if (class(mydf[,field])[1] == "POSIXt") { x = "date" }
  getVectorType = x
}


