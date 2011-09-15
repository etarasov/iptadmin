$(document).ready(function(){
    var rebuttonActionButtons = function () {
        /*
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
        */
        $(".editButton").click(editButtonHandler);
        $(".delButton").click(delButtonHandler);
        $(".insertButton").click(insertButtonHandler);
        $(".addButton").click(addButtonHandler);
        $(".editChainButton").click(editChainButtonHandler);
        $(".addChainButton").click(addChainButtonHandler);
        $(".delChainButton").click(delChainButtonHandler);
        $(".editPolicyButton").click(editPolicyButtonHandler);
    };

    var ajaxError = function () {
        alert('Server connection error');
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
                                error: ajaxError,
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
                                error: ajaxError,
                                success: function (ans2) {
                                    if (ans2 == "ok") {
                                        dialog1.dialog("destroy");
                                        $.ajax({
                                            type: "GET",
                                            url: '/show/rule?table='+table+'&chain='+chain+'&pos='+rulePos,
                                            error: ajaxError,
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
            error: ajaxError,
        });
    };

    var delButtonHandler = function () {

        var rulePos = this.getAttribute('data-rulePos');
        var chain = this.getAttribute('data-chain');
        var table = this.getAttribute('data-table');

        var dialog1;
        $("#dialog1").html("Delete rule "+rulePos+" from '"+chain+"' chain in '"+table+"' table?");

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
                            error: ajaxError,
                            success: function (ans4) {
                                if (ans4 == "ok") {
                                    dialog1.dialog("destroy");
                                    $.ajax({
                                        type: "GET",
                                        url: '/show/chain?table='+table+'&chain='+chain,
                                        error: ajaxError,
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
                        dialog1.dialog("destroy"); // было "close"
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
            error: ajaxError,
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
                                    error: ajaxError,
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
                                    error: ajaxError,
                                    success: function (ans9) {
                                        if (ans9 == "ok") {
                                            dialog1.dialog("destroy");
                                            $.ajax({
                                                type: "GET",
                                                url: '/show/chain?table='+table+'&chain='+chain,
                                                error: ajaxError,
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

    var addButtonHandler = function () {
        var dialog1;

        var chain = this.getAttribute('data-chain');
        var table = this.getAttribute('data-table');

        $.ajax({
            url: '/add?table='+table+'&chain='+chain,
            dataType: 'html',
            error: ajaxError,
            success: function (ans) {
                $('#dialog').html("");
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
                                    url: "/add",
                                    data: str,
                                    error: ajaxError,
                                    success: function (ans) {
                                        dialog1.html(ans);
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
                                    url: "/add",
                                    data: str,
                                    error: ajaxError,
                                    success: function (ans) {
                                        if (ans == "ok" ) {
                                            dialog1.dialog("destroy");
                                            // TODO: load only one rule and add it to chain
                                            $.ajax({
                                                type: "GET",
                                                url: '/show/chain?table='+table+'&chain='+chain,
                                                error: ajaxError,
                                                success: function (ans) {
                                                    $('#chain-table-'+table+'-'+chain).replaceWith(ans);
                                                    rebuttonActionButtons();
                                                },
                                            })
                                        }
                                        else {
                                            dialog1.html(ans);
                                            dialog1.dialog("option","width",'auto');
                                            dialog1.dialog("option","height",'auto');
                                            dialog1.dialog("option","position","center");
                                        }
                                    },
                                });
                            },
                        },
                    ],
                });

                dialog1.html(ans);
                dialog1.dialog("option","width",'auto');
                dialog1.dialog("option","height",'auto');
                dialog1.dialog("option","position","center");
            },
        });
    };

    var editChainButtonHandler = function () {
        var dialog1;

        var chain = this.getAttribute('data-chain');
        var table = this.getAttribute('data-table');

        $.ajax({
            url: '/editchain?table='+table+'&chain='+chain,
            dataType: 'html',
            error: ajaxError,
            success: function (ans) {
                $('#dialog').html("");
                dialog1 = $('#dialog1').dialog({
                    title: "Edit chain name",
                    modal: true,
                    resizable: false,
                    buttons: [
                        {
                            text: "Check",
                            click: function () {
                                var str = $('#editChainForm').serialize();
                                str = str + '&submit=Check';
                                $.ajax({
                                    type: "POST",
                                    url: "/editchain",
                                    data: str,
                                    error: ajaxError,
                                    success: function (ans) {
                                        dialog1.html(ans);
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
                                var str = $('#editChainForm').serialize();
                                str = str + '&submit=Submit';
                                $.ajax({
                                    type: "POST",
                                    url: "/editchain",
                                    data: str,
                                    error: ajaxError,
                                    success: function (ans) {
                                        if (ans.split(':')[0] == "ok") {
                                            // "ok:NewChainName
                                            newChain = ans.split(':')[1];
                                            dialog1.dialog("destroy");
                                            $.ajax({
                                                type: "GET",
                                                url: '/show/chain?table='+table+'&chain='+newChain,
                                                error: ajaxError,
                                                success: function (ans) {
                                                    $('#chain-table-'+table+'-'+chain).replaceWith(ans);
                                                    rebuttonActionButtons();
                                                },
                                            });
                                        }
                                        else {
                                            dialog1.html(ans);
                                            dialog1.dialog("option","width",'auto');
                                            dialog1.dialog("option","height",'auto');
                                            dialog1.dialog("option","position","center");
                                        }
                                    },
                                });
                            },
                        },
                    ],
                });

                dialog1.html(ans);
                dialog1.dialog("option","width",'auto');
                dialog1.dialog("option","height",'auto');
                dialog1.dialog("option","position","center");
            },
        });
    };

    var addChainButtonHandler = function () {
        var dialog1;

        var table = this.getAttribute('data-table');

        $.ajax({
            url: '/addchain?table='+table,
            dataType: 'html',
            error: ajaxError,
            success: function (ans) {
                $('#dialog').html("");
                dialog1 = $("#dialog1").dialog({
                    title: "Add user defined chain",
                    modal: true,
                    resizable: false,
                    buttons: [
                        {
                            text: "Check",
                            click: function () {
                                var str=$('#editChainForm').serialize();
                                str = str + '&submit=Check';
                                $.ajax({
                                    type: "POST",
                                    url: "/addchain",
                                    data: str,
                                    error: ajaxError,
                                    success: function (ans) {
                                        dialog1.html(ans);
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
                                var str = $('#editChainForm').serialize();
                                str = str + '&submit=Submit';
                                $.ajax({
                                    type: "POST",
                                    url: "/addchain",
                                    data: str,
                                    error: ajaxError,
                                    success: function (ans) {
                                        if (ans.split(':')[0] == "ok") {
                                            newChain = ans.split(':')[1];
                                            dialog1.dialog("destroy");
                                            $.ajax({
                                                type: "GET",
                                                url: '/show/chain?table='+table+'&chain='+newChain,
                                                error: ajaxError,
                                                success: function (ans) {
                                                    // TODO: добавление новой цепочки в таблицу
                                                    $('#addChainButton').before(ans);
                                                    rebuttonActionButtons();
                                                },
                                            });
                                        }
                                        else {
                                            dialog1.html(ans);
                                            dialog1.dialog("option","width",'auto');
                                            dialog1.dialog("option","height",'auto');
                                            dialog1.dialog("option","position","center");
                                        }
                                    },
                                });
                            },
                        },
                    ],
                });
                dialog1.html(ans);
                dialog1.dialog("option","width",'auto');
                dialog1.dialog("option","height",'auto');
                dialog1.dialog("option","position","center");
            },
        });
    };

    var delChainButtonHandler = function () {
        var dialog1;

        var table = this.getAttribute('data-table');
        var chain = this.getAttribute('data-chain');

        $("#dialog1").html("Delete chain '" + chain + "'?");
        dialog1 = $("#dialog1").dialog({
            title: "Del user defined chain",
            modal: true,
            resizable: false,
            buttons: [
                {
                    text: "Delete",
                    click: function () {
                        $.ajax({
                            type: "POST",
                            url: "/delchain",
                            data: 'table='+table+'&chain='+chain,
                            error: ajaxError,
                            success: function (ans) {
                                if (ans == "ok") {
                                    dialog1.dialog("destroy");
                                    $('#chain-table-'+table+'-'+chain).remove();
                                }
                                else {
                                    dialog1.html(ans);
                                }
                            },
                        });
                    },
                },
                {
                    text: "Cancel",
                    click: function () {
                        dialog1.dialog("destroy");
                    },
                },
            ],
        });
    };

    var editPolicyButtonHandler = function () {
        var dialog1;

        var chain = this.getAttribute('data-chain');
        var table = this.getAttribute('data-table');

        $.ajax({
            url: '/editpolicy?table='+table+'&chain='+chain,
            dataType: 'html',
            error: ajaxError,
            success: function (ans) {
                $('#dialog1').html("");
                dialog1 = $('#dialog1').dialog({
                    modal: true,
                    resizable: false,
                    buttons: [
                        {
                            text: "Submit",
                            click: function () {
                                var str = $("#editPolicyForm").serialize();
                                str = str + "&submit=Submit",
                                $.ajax({
                                    type: "POST",
                                    url: "/editpolicy",
                                    data: str,
                                    error: ajaxError,
                                    success: function (ans) {
                                        if (ans.split(':')[0] == "ok") {
                                            dialog1.dialog("destroy");
                                            var newPolicy = ans.split(':')[1];
                                            $('#policy-'+table+'-'+chain).html(newPolicy);
                                        }
                                        else {
                                            dialog1.html(ans);
                                            dialog1.dialog("option","width",'auto');
                                            dialog1.dialog("option","height",'auto');
                                            dialog1.dialog("option","position","center");
                                        }
                                    },
                                });
                            },
                        },
                    ],
                });
                dialog1.html(ans);
                dialog1.dialog("option","width",'auto');
                dialog1.dialog("option","height",'auto');
                dialog1.dialog("option","position","center");
            },
        });
    };

    $('body').append('<div id="dialog1"></div>');
    rebuttonActionButtons();
});
