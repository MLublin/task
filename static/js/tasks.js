
$(document).ready(function() {

    $("#newtaskform").hide();

    $(".taskbullet").click(function() {
      var complete = confirm("Set task complete?");
      var tid = $(this).attr("id");
      if (complete) $("#form" + tid).submit();
    });

    $("#newtaskbttn").click(function() {
      $("#newtaskform").toggle(); 
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
	  var user = $("#username").text(); 
	  var members = newtask.members;
	  var destination;
	  console.log("user: " + user + "members: " + members);
	  if ($.inArray(user, members) != -1) {
		  var priority = newtask.priority;
		  destination = $("#tasks"+priority);
		  if (!$("#curtasks").length) {
		    var curtaskheader =  '<h4 id="curtasks"> In progress:  </h4>';
		    $("#tasks").prepend(curtaskheader);
		  }
		  if (!$("#taskheader").length) {
		    var taskheader =  '<h3 id="taskheader"> My tasks </h3>';
		    $("#tasks").prepend(taskheader);
		  }            
		  var headerId = formatPriority(priority) + "Header";
		  if (!$("#" + headerId).length) {
		    console.log(headerId);
		    console.log("Task div: " + $("#tasks" + priority));
		    var curpriorityheader =  '<h5 class="blue" id="' + headerId + '">' + formatPriority(priority) + ' Priority  </h5>';
		    $("#tasks" + priority).prepend(curpriorityheader);
		  }
	  } else {
            destination = $("#other_tasks");
	      if (!$("#othertasksheader").length) {
	        var othertaskheader = '<h3 id="othertasksheader"> Other tasks </h3>';
	        $("#other_tasks").prepend(othertaskheader);
	      } 
	  }
          var html = 
          $('<li class="task" id="' + tid + '">' +
            newtask.name + '<br>' +
            '<blockquote> Members: ' + printArray(newtask.members) + '</blockquote></li>' + 
            '<form id="form' + tid + '" action="/tasks/' + tid + '/edit" method="post">' 
            + '<input type="hidden" name="completed" value="True">' ).appendTo(destination);
          $(destination).append(html);
          console.log("appended to destination: " + destination);
          $("#newtaskform").find("textarea").val("");
          $("#"+tid).click(function() {
            var complete = confirm("Set task complete?");
            var tid = $(this).attr("id");
            if (complete) $("#form" + tid).submit();
          });
          $("#newtaskform").hide();
          return data;
        }
      });
      $("#newtaskform").hide();
      return false;
    });
});

function printArray (array) {
  var str = "";
  for (var i = 0; i < array.length; i++) {
    str += (" " + array[i]);
  }
  return str;
}

// Convert task priority from 1/2/3 to high/med/low
function formatPriority(priority) {
  if (priority == "1") return "High";
  if (priority == "2") return "Medium";
  if (priority == "3") return "Low";
  return "Not set";
}

