<script src="js/highlight.pack.js"></script>
<link href="css/dkknitr.css" rel="stylesheet"/>
<script src="js/toc.js"></script>

<link href="js/select2/select2.css" rel="stylesheet"/>
<script src="js/select2/select2.js"></script>

<script>
Shiny.addCustomMessageHandler("fieldsloaded",
  function(message) {
    console.log("fieldsloaded")
    $("#numerics").select2({ width: 'resolve' });
    $("#factors").select2({ width: 'resolve' });
    $("#dates").select2({ width: 'resolve' });
    $("#logicals").select2({ width: 'resolve' });
  }
);
</script>
