$(document).ready(function() {
    $('.leaderCheckbox').hide();
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
                    console.log("show " + $(this).val());
                    showIt = true;
                    $(this).show();
                    break;
                 }
            }
            var classstr = '.' + $(this).val();
            if(showIt){
                $(classstr).show();

            } else {
                $(classstr).hide();
            }
        });
    });
});