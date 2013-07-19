
$(document).ready(function() {

  // update user list to include new user
  var cuser = $("#currentUser").text();
  var userHtml = $('<div id="' + cuser + '">' + cuser + '</div>').appendTo("#users");
  $("#users").append(userHtml);

  var nchats = 0; // number of chats currently indexed on the page

  function poll() {
    setTimeout(function() {
      $.ajax({
        url: document.URL,
        success: function(data) {
          console.log(data);
          var firstChat = nchats; // number of first chat
          console.log("firstChat: " + firstChat);
          for (var n = nchats; n < data.length; n++) {
            $("#chats").append(showChat(data[n], n));
          }
          nchats = data.length;
          var maxchats = 10;
          var extras = nchats - maxchats; // number of chats to delete to display maxchats
          console.log("nchats: " + nchats + "; extras: " + extras + "; firstChat: " + firstChat);
          if (extras > 0) {
            for (var i = 0; i < extras; i++) {
              $("#"+ (i + firstChat)).remove();
            }
          }
          console.log("nchats: " + nchats);
          poll();
        },
        dataType: "json"
      });
    }, 750);  // refresh every 0.75 sec
  }

  poll();

  $("#chatForm").submit(function() {
    var dataString = $("#chatForm").serialize();
    var text = $("#text").val();
    if ((text != '') && (text != null) && (text != "undefined")) {
      $.ajax({
        dataType: "json",
        type: "POST",
        url: "",
        data: dataString,
        success: function(data) {
          var newchat = data[data.length - 1];
          $("#text").val("");
          poll();
          return data;
        },
      });
    }
    return false;
  });

  $(window).unload(function() {  // user left, so remove their name from the list
    console.log("removing cuser: " + cuser);
    $("#"+cuser).remove();
  });

});


/**
Returns the formatted chat ready to be appended to the chats div
*/
function showChat(chat, number) {  // number = which chat it is (#1, 2, 3, ...)
  var cid = chat._id;
  var timestamp = cid.toString().substring(0,8);
  var date = formatDate(new Date(parseInt(timestamp, 16) * 1000));
  var html =
  $('<p id="' + number + '">' + "<" + date + ">" + '<b>' + chat.author + ": " + '</b>' + 
    chat.text + '</p>').appendTo("#chats");
}

/**
Format the date to something like "16:31:09"
*/
function formatDate(d) {
  var h = d.getHours();
  var m = addZero(d.getMinutes());
  var s = addZero(d.getSeconds());
  return h + ":" + m + ":" + s;
}

function addZero(n) { // to match the haskell format
  if (n < 10) {
    n = "0" + n;
  }
  return n;
}

