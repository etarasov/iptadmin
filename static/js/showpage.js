$(document).ready(function(){
    var editButtonHandler = function () {
        var dialog1;

        var loadEditForm = function (ans) {
            //alert('loadEditForm');
            //console.log('loadEditForm');
            //console.log(ans);

            //alert('1');
            dialog1 = $('#dialog1').dialog({
                modal: true,
                resizable: false,
                buttons: [
                    {
                        text: "Check",
                        click: function () {
                            var str = $("#editform").serialize();
                            str = str + "&submit=Check";
                            $.ajax({
                                type: "POST",
                                url: "/edit",
                                data: str,
                                success: function (ans2) {
                                    dialog1.html(ans2);
                                    dialog1.dialog("option","width",'auto');
                                    dialog1.dialog("option","height",'auto');
                                    dialog1.dialog("option","position","center");
                                },
                            });
                        },
                    },
                    {
                        text: "Submit",
                        click: function () {
                            var str = $("#editform").serialize();
                            str = str + "&submit=Submit";
                            $.ajax({
                                type: "POST",
                                url: "/edit",
                                data: str,
                                success: function (ans2) {
                                    if (ans2 == "ok") {
                                        dialog1.dialog("destroy");
                                        $.ajax({
                                            type: "GET",
                                            url: '/show/rule?table='+table+'&chain='+chain+'&pos='+rulePos,
                                            success: function (ans3) {
                                                $('#rule-tr-'+table+'-'+chain+'-'+rulePos).replaceWith(ans3);
                                                $(".editButton").button({
                                                    icons: {
                                                        primary: "ui-icon-wrench"
                                                    },
                                                    text: false
                                                }).click(editButtonHandler);
                                            },
                                        });
                                    }
                                    else {
                                        dialog1.html(ans2);
                                        dialog1.dialog("option","width",'auto');
                                        dialog1.dialog("option","height",'auto');
                                        dialog1.dialog("option","position","center");
                                    }
                                },
                            });
                        },
                    },
                ],
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
                    alert("Server connection error");
                },
        });
    };


    $('body').append('<div id="dialog1"></div>');
    $(".editButton").button({
        icons: {
            primary: "ui-icon-wrench"
        },
        text: false
    }).click(editButtonHandler);

});
