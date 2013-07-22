
$(document).ready(function() {

    $(".task").click(function() {
      var complete = confirm("Set task complete?")
      var tid = $(this).attr("id");
      if (complete) $("#form" + tid).submit();
    });
});

