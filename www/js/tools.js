<script src="js/highlight.pack.js"></script>
<script src="js/toc.js"></script>

<script>
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
</script>

<link href="css/dkknitr.css" rel="stylesheet"/>