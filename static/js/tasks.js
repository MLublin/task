
$(document).ready(function() {

    $("#newprojectform").validate({
      rules: {
        title: "required",
        desc: "required",
        members: "required",
        leaders: "required",
        // startTime: "required",
        // endTime: "required"
      },
      invalidHandler: function(event, validator){
        alert("invalid form");
      }
    });

    $("#newtaskform").hide();


   $(".complete_tasks_form").submit(function(e){
          e.preventDefault();
          var form = $(this);
          removeItem(form);
          
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



    $(".removenotif").submit(function(e) {
      e.preventDefault();
      var form = $(this);
      $.ajax({
        dataType: "json",
        type: "POST",
        contentType: "text/json",
        url: form.attr("action"),
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
                  if (!$("#taskheader").length) {
                    var taskheader =  '<h3 id="taskheader"> My tasks </h3>';
                    $("#tasks").prepend(taskheader);
                  }            
                  var headerId = formatPriority(priority) + "Header";
                  if (!$("#" + headerId).length) {
                    console.log(headerId);
                    console.log("Task div: " + $("#tasks" + priority));
                    var curpriorityheader =  '<h5 class="blue headertp' + priority + '" id="' + headerId + '">' + formatPriority(priority) + ' Priority  </h5>';
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
            '<form id="form' + tid + '" class="complete_tasks_form taskp' + newtask.priority + '" action="/tasks/' + tid + '/edit" method="post">' + 
            '<button type="submit" > X </button>' +
            newtask.name + '<br>' +
            '<blockquote> Members: ' + printArray(newtask.members) + '</blockquote></li>' + 
            '<input type="hidden" name="completed" value="True">' + 
            '</form> </li>').appendTo(destination);
          $(destination).append(html);
          console.log("appended to destination: " + destination);
          $("#newtaskform").find("textarea").val("");
          $("#form" + tid).submit(function(e){
            e.preventDefault();
            var form = $(this);
            removeItem(form);
            
            // var newForm = $('<form id="complete'+ tid +'" class="remove_tasks_form" action="/tasks/'+ 
            //   tid +'/remove" method="post"> <button type="submit" > X </button>' + 
            //   ' ')
          });

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

function removeItem(form){
  console.log("remove item called");
  console.log(form.attr("action"));
  $.ajax({
    dataType: "json",
    type: "POST",
    contentType: "text/json",
    url: form.attr("action"),
    data: form.serialize(),
    error: function(jqXHR, textStatus, errorThrown) {
      alert("ajax error");
    },
    success: function(data) {
      console.log("remove item success");
      var tid = (form.attr('id')).substring(4);
      var newForm = form.clone();
      form.remove();
      newForm.attr('class', 'remove_tasks_form');
      newForm.attr('action','/tasks/' + tid + '/remove');
      newForm.attr('id', 'complete' + tid);
      console.log($(".remove_tasks_form").length);
      if ($(".remove_tasks_form").length === 0) {
        $("#complete_tasks").prepend("<h4>Completed: </h4>");
        console.log("prepend attempted");
      }
      for(var i = 1; i <= 3 ; i++ ){
        if($(".taskp" + i).length === 0) $(".headertp" + i).remove();
      }
      newForm.appendTo("#complete_tasks");
    }
  });  
}