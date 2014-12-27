Shiny.addCustomMessageHandler("showmsg",
  function(message) {
    $("#kniting").show();
  }
);
Shiny.addCustomMessageHandler("hidemsg",
  function(message) {
    $("#kniting").hide();
  }
);

/*    ## Deprecated: old knitr progress code
    # In ui.R:
    # tagList(singleton(tags$head(tags$script(src="js/customProgress.js")))),
    # p(div(id="kniting", class="alert alert-info", style="display: none", "Kniting report"))
    # In server.r:
    # session$sendCustomMessage(type="showmsg", list(mymsg="Hello"))
    # session$sendCustomMessage(type="hidemsg", list(mymsg="Hello"))
*/
