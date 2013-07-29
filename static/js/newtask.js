
$(document).ready(function() {
  console.log("newtask.js loaded");
  $("#newtaskform").hide();

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
        var destination = formatHeaders(newtask);
        var html =
        $('<li class="task" id="' + tid + '">' +
          '<form id="form' + tid + '" class="complete_tasks_form taskp' + newtask.priority +
          '" action="/tasks/' + tid + '/edit" method="post">' + 
          '<button type="submit" > X </button>' + newtask.name + '<br>' +
          '<blockquote> Members: ' + printArray(newtask.members) + '</blockquote></li>' + 
          '<input type="hidden" name="completed" value="True">' + 
          '</form> </li>').appendTo(destination);
        $(destination).append(html);
        console.log("appended to destination: " + destination);
        $("#newtaskform").hide();
        $("#newtaskform").find("textarea").val("");
        $("#form" + tid).submit(function(e){
          e.preventDefault();
          var form = $(this);
          removeItem(form);
        });
     }
   });
 });
});

/*
* Append the proper headers ("My tasks", "Low/Med/High Priority", etc) to the page. 
* Return the destination div that the new task should be appended to.
*/
function formatHeaders(newtask) {
  var user = $("#username").text();  // current user
  var members = newtask.members;
  console.log("user: " + user + "members: " + members);
  var destination;
  if ($.inArray(user, members) != -1) {  // user is a member of the task; destination is in my tasks
    var priority = newtask.priority;
    destination = $("#tasks"+priority);
    if (!$("#taskheader").length) {
      var taskheader =  '<h3 id="taskheader"> My tasks </h3>';
      $("#tasks").prepend(taskheader);
    }            
    var headerId = formatPriority(priority) + "Header";
    if (!$("#" + headerId).length) {
      console.log(headerId);
      console.log("Task div: " + $("#tasks" + priority));
      var curpriorityheader = 
        '<h5 class="blue headertp' + priority + '" id="' + headerId + '">' + 
        formatPriority(priority) + ' Priority</h5>';
      $("#tasks" + priority).prepend(curpriorityheader);
    }
  } else {  // user is not member of the task; destination is #other_tasks
    destination = $("#other_tasks");
    if (!$("#othertasksheader").length) {
      var othertaskheader = '<h3 id="othertasksheader"> Other tasks </h3>';
      $("#other_tasks").prepend(othertaskheader);
    } 
  }
  return destination;
}

