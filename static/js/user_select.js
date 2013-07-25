
$(document).ready(function() {
    $('.leaderCheckbox').hide();
    /*$('.leaderCheckbox :checkbox').each(function() {
      if ($(this).attr("checked") != "checked") {
        $(this).parent().hide();
      }
    });*/
    console.log("checkbox script loaded");
    var selected = [];
    $('#memberSelect input:checked').each(function(){
        console.log($(this).attr('name'));
        selected.push($(this).val());
    });
    console.log(selected[0]);
    $('#leaderSelect input:checkbox').each(function(){
        var showIt = false;
        for (var i = selected.length - 1; i >= 0; i--) {
            console.log(selected[i]);
             if($(this).val() == selected[i]){
                console.log("show " + $(this).val());
                showIt = true;
                break;
             }
        }
        var classstr = '.' + $(this).val();
        if(showIt){
            $(classstr).show();

        } else {
            $(classstr).hide();
            $(classstr + ' input:checkbox').removeAttr("checked");
        }
    });
    $('.memberCheckbox').click(function() {
        var selected = [];
        $('#memberSelect input:checked').each(function(){
            console.log($(this).attr('name'));
            selected.push($(this).val());
        });
        console.log(selected[0]);
        $('#leaderSelect input:checkbox').each(function(){
            var showIt = false;
            for (var i = selected.length - 1; i >= 0; i--) {
                console.log(selected[i]);
                 if($(this).val() == selected[i]){
                    console.log("show " + $(this).val());
                    showIt = true;
                    break;
                 }
            }
            var classstr = '.' + $(this).val();
            if(showIt){
                $(classstr).show();

            } else {
                $(classstr).hide();
                $(classstr + ' input:checkbox').removeAttr("checked");
            }
        });
    });
});
