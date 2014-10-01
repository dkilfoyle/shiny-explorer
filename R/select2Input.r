# LEGACY CODE - now use Shiny 0.9+ selectize with drag_drop plugin
# eg selectizeInput("idnumerics", "My Label", choices=c("Hello","Bonjour", "etc"), selected="", multiple=T,
#                options=list(placeholder="Select Greeting", dropdownParent = "body", plugins=list(remove_button="", drag_drop="")))

select2Input <- function(inputId, label, choices = NULL, selected = NULL, placeholder = "", ...) {

  tagList(
    
    singleton(tags$head(tags$link(href="js/select2/select2.css",rel="stylesheet",type="text/css"))),
    singleton(tags$head(tags$script(src="js/select2/select2.js"))),
    singleton(tags$head(tags$script(src="js/jquery-ui-1.10.3.custom.min.js"))),
    singleton(tags$head(tags$script(src="js/select2.sortable.js"))),
    
    selectInput(inputId, label, choices, selected, selectize=F, ...),
    tags$script(sprintf("$(function() { $('#%s').select2({width:'resolve', placeholder:'%s'}); $('#%s').select2Sortable(); })", inputId, placeholder, inputId))

  )
}