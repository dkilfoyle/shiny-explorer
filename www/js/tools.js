<script src="js/highlight.pack.js"></script>
<link href="css/dkknitr.css" rel="stylesheet"/>
<script src="js/toc.js"></script>

<script>
/*
// How to send javascript from R to client
// R: session$sendCustomMessage(type='fieldsloaded', list(mymsg="Hello"))
Shiny.addCustomMessageHandler("fieldsloaded",
  function(message) {
    console.log("fieldsloaded")
    $("#numerics").select2({ width: 'resolve' });
    $("#factors").select2({ width: 'resolve' });
    $("#dates").select2({ width: 'resolve' });
    $("#logicals").select2({ width: 'resolve' });
  }
);*/
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
$(document).ready(function() {
  $("#kniting").hide(); 
});
</script>

<link href="js/select2/select2.css" rel="stylesheet"/>
<script src="js/select2/select2.js"></script>

<script>
/* implement select2 support for the field selects */
$(document).ready(function() {
  $("#numerics").select2({ width: 'resolve', placeholder: 'Select numeric(s)' }); 
  $("#factors").select2({ width: 'resolve', placeholder: 'Select factor(s)' });
  $("#dates").select2({ width: 'resolve', placeholder: 'Select date(s)' });
  $("#logicals").select2({ width: 'resolve', placeholder: 'Select logical(s)' }); 
});
</script>