
$(document).ready(function() {

  $("#newCommentForm").submit(function() {
    var dataString = $("#newCommentForm").serialize();
    var pid = $("#post").val();
    var text = $("#text").val();
    if ((text != '') && (text != null) && (text != "undefined")) {
      $.ajax({
        dataType: "json",
        type: "POST",
        url: "",
        data: dataString,
        success: function(data) {
          var array = data;
          var newcomment = array[array.length - 1];
          showComment(newcomment, "#root");
          return data;
        },
      });
    }
    return false;
  });
  $(".reply-button").click(function() {
    var parent = $(this).parent()[0]; // returns a div
    handle_reply(parent);
    $(this).remove();
  });
  $(".edit-button").click(function() {
    var id = this.id.substring(2);
    var parent = $("#"+id);
    handle_edit(parent, id);
    $(this).remove();
  });
});

/**
Display a form allowing user to edit a comment, and provide a callback function
*/
function handle_edit(oldcomment, id) {
  var username = $("#username").text();
  if (username == "Anonymous") {
    return false;
  }
  var url = document.URL.split("/");
  if (url.length < 2) {
    return false;
  }
  var postid = url[url.length - 2]; // url is x.org/postid/comments
  var parent = $("#p"+id).text();
  var form =
  $('<form action="#">'+
    '<input type="hidden" name="_id" value="' + id + '"/>'+
    '<input type="hidden" name="method" value="PUT"/>'+
    '<input type="hidden" name="parent" value="' + parent + '"/>' +
    '<input type="hidden" name="post" value="' + postid + '"/>'+
    '<input type="hidden" name="author" value="' + username + '"/>'+
    '<input type="hidden" name="edited" value="True"/>'+
    '<textarea name="text"></textarea><br>'+
    '<input type="submit" value="Edit"/>'+
    '</form>').appendTo("#text"+id);

  $("#text"+id).append(form);

  form.submit(function(event) {
    event.preventDefault();
    var dataString = form.serialize();
    var f = $(this);
    $.ajax({
      dataType: "json",
      type: "PUT",
      url: "",
      data: dataString,
      success: function(data) {
        if (data.length <= 0) {
          return false;
        }
        var target = data[data.length - 1]; // the comment that was edited
        for (var n = data.length - 1; n >= 0; n--) { 
          if (data[n]._id == id) {
            target = data[n];
          }
        }
        var destination = $("#text"+id);
        var html = 
        $('<blockquote id="text' + id + '">' + 
          target.text.replace(/\r?\n/g, '<br />') + 
          '</blockquote>').appendTo(destination);
        $(destination).replaceWith(html);
        f.remove();
        return false;
      }
    })
    return false;
  });
}

/**
Replace the old comment with the updated comment
*/
function updateComment(comment) {
  var cid = comment._id;
  var destination = $("#text"+cid);
  var html = 
  $('<blockquote id="text' + cid + '">' + 
    comment.text.replace(/\r?\n/g, '<br />') + 
    '</blockquote>').appendTo(destination);
  $(destination).replaceWith(html);
}

/**
Display a form allowing user to submit a reply, and provide a callback function
*/
function handle_reply(parent) {
  var username = $("#username").text();
  var id = parent.id; // in reply to
  var url = document.URL.split("/");
  if (url.length < 2) {
    return false;
  }
  var postid = url[url.length - 2]; // url is lbh.org/postid/comments
  var form =
  $('<form action="#">'+
    '<input type="hidden" name="parent" value="' + id + '"/>'+
    '<input type="hidden" name="post" value="' + postid + '"/>'+
    '<input type="hidden" name="author" value="' + username + '"/>'+
    '<textarea name="text"></textarea><br>'+
    '<input type="submit" value="Reply"/>'+
    '</form>').appendTo(parent);
  form.submit(function(event) {
    event.preventDefault();
    var dataString = form.serialize();
    var f = $(this);
    $.ajax({
      dataType: "json",
      type: "POST",
      url: "",
      data: dataString,
      success: function(data) {
        var array = data;
        if (array.length <= 0) {
          return false;
        }
        var newcomment = array[array.length - 1];
        showComment(newcomment, "#"+newcomment.parent);
        f.remove();
        return false;
      }
    })
    return false;
  });
}

/**
Append the comment, a reply button, and an edit button if applicable to the destination div
*/
function showComment(comment, destination) {
  var cid = comment._id;
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var buttons = '<button class="reply-button" id="rb' + cid + '">Reply</button><br>';
  var username = $("#username").text();
  if (username == comment.author && username != "Anonymous") {
    buttons = 
      '<button class="edit-button2" id="eb' + cid + '">Edit</button><br>'+
      buttons;
  }
  var html = // comment plus reply button
  $('<div class="comment" id=' + cid + '>' +
    '<h3>' + comment.author + '</h3>' +
    '<p>' + date + '</p>' +
    '<blockquote id="text' + cid + '">' + 
    comment.text.replace(/\r?\n/g, '<br />') + '</blockquote>' +
    '<li id="p' + comment._id + '">' + comment.parent + '</li>' +
    buttons +
    '</div>').appendTo(destination);
  $(destination).append(html);

  $(".reply-button").click(function() {
    var parent = $(this).parent()[0];
    handle_reply(parent);
    $(this).remove();
  });
  $(".edit-button2").click(function() {
    var id = this.id.substring(2);
    var parent = $("#"+id);
    handle_edit(parent, id);
    $(this).remove();
  });
}

/**
Format the date to something like "2013-06-10 16:31" to match the haskell format
*/
function formatDate(d) {
  var h = d.getHours();
  var min = addZero(d.getMinutes());
  var mon = addZero(d.getMonth() + 1);
  var day = addZero(d.getDate());
  return d.getFullYear() + "-" + mon + "-" + day + " " + h + ":" + min;
}

function addZero(n) { // to match the haskell format
  if (n < 10) {
    n = "0" + n;
  }
  return n;
}

