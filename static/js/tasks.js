
$(document).ready(function() {

    $("#newtaskform").hide();

    $(".task").click(function() {
      var complete = confirm("Set task complete?")
      var tid = $(this).attr("id");
      if (complete) $("#form" + tid).submit();
    });

    $("#newtaskbttn").click(function() {
      $("#newtaskform").show(); 
    });

    $("#newtaskform").submit(function(e) {
      e.preventDefault();
      console.log("new task form submitted, default prevented");
      var curpath = window.location.pathname
      console.log("current path: " + window.location.pathname);
      var dataString = $("#newtaskform").serialize();
      $.ajax({
        dataType: "json",
        type: "POST",
        contentType: "text/json",
        url: curpath + "/tasks",
        data: dataString,
        error: function(jqXHR, textStatus, errorThrown) {
          console.log(arguments);
          console.log(jqXHR, textStatus, errorThrown);
          console.log("ajax error");
          alert("ajax error");
        },
        success: function(data) {
          console.log("success");
          // var newtask = data[data.length - 1];
          var newtask = data[0];
          var tid = newtask._id;
          var priority = newtask.priority;
          var destination = $("#"+priority+"tasks");
          var html = 
          $('<li class="task" id="' + tid + '">' +
            newtask.name + '<br>' +
            'Priority: ' + formatPriority(newtask.priority) + '<br>' +
            'Members: ' + newtask.members + '</li>').appendTo(destination);
          $(destination).append(html);
          console.log("appended to destination: " + destination);
          return data;
        }
      });
      $("#newtaskform").hide();
      return false;
    });
});

// Convert task priority from 1/2/3 to high/med/low
function formatPriority(priority) {
  if (priority == "1") return "High";
  if (priority == "2") return "Medium";
  if (priority == "3") return "Low";
  return "Not set";
}

