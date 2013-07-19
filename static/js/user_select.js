$(document).ready(function() {
    console.log("checkbox script loaded");
    $('.membersCheckbox').click(function() {
        var selected = [];
        $('#userSelect input:checked').each(function(){
            console.log($(this).attr('name'));
            selected.push($(this).val());
        });
        console.log(selected[0]);
        $('#leaderSelect input:checkbox').each(function(){
            var hideIt = true;
            for (var i = selected.length - 1; i >= 0; i--) {
                console.log("in loop");
                 if($(this).val() == selected[i]){
                    hideIt = false;
                    break;
                 }
            }
            if(hideIt) $(this).hide();
    });
});