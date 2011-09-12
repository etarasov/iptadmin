$(document).ready(function(){
    var rebuttonActionButtons = function () {
        $(".editButton").button({
            icons: {
                primary: "ui-icon-wrench"
            },
            text: false
        }).click(editButtonHandler);

        $(".delButton").button({
            icons: {
                primary: "ui-icon-closethick"
            },
            text: false,
        }).click(delButtonHandler);

        $(".insertButton").button({
            icons: {
                primary: "ui-icon-plus"
            },
            text: false,
        }).click(insertButtonHandler);
    };

    var editButtonHandler = function () {
        var dialog1;

        var loadEditForm = function (ans) {

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
                                error: function () {
                                    alert('server connection error');
                                },
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
                                                rebuttonActionButtons();
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

    var delButtonHandler = function () {

        var rulePos = this.getAttribute('data-rulePos');
        var chain = this.getAttribute('data-chain');
        var table = this.getAttribute('data-table');

        var dialog1;
        $("#dialog1").html("Delete rule number "+rulePos+" from '"+chain+"' chain in '"+table+"' table.");

        dialog1 = $("#dialog1").dialog({
            modal: true,
            resizable: false,
            title: "Delete rule",
            buttons: [
                {
                    text: "Delete",
                    click: function () {
                        $.ajax({
                            type: "POST",
                            url: "/del",
                            data: 'rulePos='+rulePos+'&chain='+chain+'&table='+table,
                            success: function (ans4) {
                                if (ans4 == "ok") {
                                    dialog1.dialog("destroy");
                                    $.ajax({
                                        type: "GET",
                                        url: '/show/chain?table='+table+'&chain='+chain,
                                        success: function (ans6) {
                                            $('#chain-table-'+table+'-'+chain).replaceWith(ans6);
                                            rebuttonActionButtons();
                                        },
                                    });
                                    // alert("rule was deleted");
                                }
                                else {
                                    alert("rule deletion error");
                                };
                            },
                        });
                    },
                },
                {
                    text: "Cancel",
                    click: function () {
                        dialog1.dialog("close");
                    },
                },
            ],
        });
    };

    var insertButtonHandler = function () {
        var dialog1;

        var rulePos = this.getAttribute('data-rulePos');
        var chain = this.getAttribute('data-chain');
        var table = this.getAttribute('data-table');

        $.ajax({
            url: '/insert?table='+table+'&chain='+chain+'&pos='+rulePos,
            dataType: 'html',
            error: function () {
                    alert("Server connection error");
                    },
            success: function (ans7) {
                $('#dialog1').html("");
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
                                    url: "/insert",
                                    data: str,
                                    error: function () {
                                        alert('server connection error');
                                    },
                                    success: function (ans8) {
                                        dialog1.html(ans8);
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
                                    url: "/insert",
                                    data: str,
                                    error: function () {
                                        alert('server connection error');
                                    },
                                    success: function (ans9) {
                                        if (ans9 == "ok") {
                                            dialog1.dialog("destroy");
                                            $.ajax({
                                                type: "GET",
                                                url: '/show/chain?table='+table+'&chain='+chain,
                                                error: function () {
                                                    alert('server connection error');
                                                },
                                                success: function (ans) {
                                                    $('#chain-table-'+table+'-'+chain).replaceWith(ans);
                                                    rebuttonActionButtons();
                                                },
                                            });
                                        }
                                        else {
                                            dialog1.html(ans2);
                                            dialog1.dialog("option","width",'auto');
                                            dialog1.dialog("option","height",'auto');
                                            dialog1.dialog("option","position","center");
                                        };
                                    },
                                });
                            },
                        },
                    ],
                });
                dialog1.html(ans7);
                dialog1.dialog("option","width",'auto');
                dialog1.dialog("option","height",'auto');
                dialog1.dialog("option","position","center");
            },
        });

    };

    $('body').append('<div id="dialog1"></div>');
    rebuttonActionButtons();
});
