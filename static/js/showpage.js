$(document).ready(function(){
    $(".editButton").button({
        icons: {
            primary: "ui-icon-wrench"
        },
        text: false
    }).click( function() {

        var dialog1;

        var loadEditForm = function (ans) {
            //alert('loadEditForm');
            //console.log('loadEditForm');
            //console.log(ans);

            $('body').append('<div id="dialog1"></div>');
            //alert('1');
            dialog1 = $('#dialog1').dialog({
                modal: true,
                resizable: false,
                //close: closeDialog,
            });
            //alert('2');
            dialog1.html(ans);
            //alert('3');
            dialog1.dialog("option","width",'auto');
            dialog1.dialog("option","height",'auto');
            dialog1.dialog("option","position","center");
        };
        var rulePos = this.getAttribute('data-rulePos');
        var chain = this.getAttribute('data-chain');
        var table = this.getAttribute('data-table');

        $.ajax({
            url: '/edit?table='+table+'&chain='+chain+'&pos='+rulePos,
            dataType: 'html',
            success: loadEditForm,
            error: function () {
                    alert("ahtung");
                }
        });


    });
});
