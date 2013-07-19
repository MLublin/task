
$(document).ready(function() {

    $(".task").click(function() {
      var tid = $(this).attr("id");
      console.log("id: " + tid);
      $("#form" + tid).submit();
      console.log("form submitted");
    });
});

