$(document).ready(function() {
    $('.leaderCheckbox').hide();
    $('.memberCheckbox').click(function() {
        var selected = [];
        $('#memberSelect input:checked').each(function(){
            selected.push($(this).val());
        });
        $('#leaderSelect input:checkbox').each(function(){
            var showIt = false;
            for (var i = selected.length - 1; i >= 0; i--) {
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