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