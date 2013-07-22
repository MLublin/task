$(document).ready(function() {
    console.log("checkbox script loaded");
    $('.memberCheckbox').click(function() {
        var selected = [];
        console.log("eeeeeee");
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
                    showIt = true;
                    break;
                 }
            }
            var idstr = '#' + $(this).val();
            if(showIt){
                $(idstr).show();
            } else {
                $(idstr).hide();
            }
        });
    });
});