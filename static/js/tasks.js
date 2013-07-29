
$(document).ready(function() {

  // Validate new project form: all fields required except end time
  $("#newprojectform").validate({
    rules: {
      title: "required",
      desc: "required",
      members: "required",
      leaders: "required",
      startTime: "required",
    },
    invalidHandler: function(event, validator){
      alert("invalid form");
    }
  });

  // Mark task as completed
  $(".complete_tasks_form").submit(function(e){
    e.preventDefault();
    var form = $(this);
    removeItem(form);
  });

  // Clear all notifications
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

  // Remove one notification
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
});

// Remove the form (which contains a task) from the incomplete_tasks div and append it to the complete_tasks div
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
      form.hide();
      newForm.attr('class', 'remove_tasks_form');
      newForm.attr('action','/tasks/' + tid + '/remove');
      newForm.attr('id', 'complete' + tid);
      newForm.appendTo("#complete_tasks");
    }
  });  
}

// Display objects inside the array as a space-separated string
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

