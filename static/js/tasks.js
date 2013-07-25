
$(document).ready(function() {

    $("#newtaskform").hide();

    $(".taskbullet").click(function(e) {
      e.preventDefault();
      var complete = confirm("Set task complete?");
      var tid = $(this).attr("id");
      if (complete) completeTask(tid);
    });

    $("#removeallnotifs").submit(function(e) {
      e.preventDefault();
      $.ajax({
        dataType: "json",
        type: "POST",
        contentType: "text/json",
        url: $("#removeallnotifs").attr("action"),
        data: "",
        error: function(jqXHR, textStatus, errorThrown) {
          alert("ajax error");
        },
        success: function(data) {
          $("#notifications").remove();
        }
      });
    });

    $("#removenotif").submit(function(e) {
      var form = $(this);
      e.preventDefault();
      $.ajax({
        dataType: "json",
        type: "POST",
        contentType: "text/json",
        url: $("#removenotif").attr("action"),
        data: "",
        error: function(jqXHR, textStatus, errorThrown) {
          alert("ajax error");
          console.log(textStatus, errorThrown);
        },
        success: function() {
          form.remove();
        }
      });
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

              $("#"+tid).click(function(e) {
                e.preventDefault();
                var complete = confirm("Set task complete?");
                if (complete) completeTask(tid);

          });
          return data;
        }
      });
      $("#newtaskform").hide();
      return false;
    });
});

/* AJAX call to set task to completed 
 */
function completeTask (tid){
    var dataString = $("#form" + tid).serialize();
    var task = $("#" + tid);
    $.ajax({
        dataType: "json",
        type: "POST",
        contentType: "text/json",
        url: "/tasks/" + tid + "/edit",
        data: dataString,
        error: function(jqXHR, textStatus, errorThrown) {
          console.log(arguments);
          console.log(jqXHR, textStatus, errorThrown);
          console.log("ajax error");
          alert("ajax error");
        },
        success: function(){
          task.remove();
          $("#complete_tasks").append(task);
          for (var i = 1; i <= 3; i++) {
            console.log("the length is:  "+$("#tasks" + i).length);
            if($("#tasks" + i).length == 1) $("#tasks" + i).remove();
          }
          if($("#incomplete_tasks").length == 1) $("#incomplete_tasks").empty();

        }
      });
}

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

