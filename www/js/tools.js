<script src="js/highlight.pack.js"></script>
<script src="js/toc.js"></script>
<script src="js/jquery-ui-1.10.3.custom.min.js"></script>

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

<link href="js/select2/select2.css" rel="stylesheet"/>
<script src="js/select2/select2.js"></script>
<script src="js/select2.sortable.js"></script>

<script>
/* LEGACY CODE - now use shiny 0.9+ selectizeInput
$(document).ready(function() {
  $("#numerics").select2({ width: 'resolve', placeholder: 'Select numeric(s)' }); $("#numerics").select2Sortable()
  $("#factors").select2({ width: 'resolve', placeholder: 'Select factor(s)' }); $("#factors").select2Sortable()
  $("#dates").select2({ width: 'resolve', placeholder: 'Select date(s)' }); $("#dates").select2Sortable()
  $("#logicals").select2({ width: 'resolve', placeholder: 'Select logical(s)' }); $("#logicals").select2Sortable()
});
*/
</script>

<link href="css/dkknitr.css" rel="stylesheet"/>