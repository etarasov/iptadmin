$(document).ready(function(){
    $(".editButton").button({
        icons: {
            primary: "ui-icon-wrench"
        },
        text: false
    }).click( function() {
        $('body').append('<div id="dialog1">asdfadsf</div>');
        $('#dialog1').dialog({
            modal: true,
            //close: closeDialog,
        });
    });
});
