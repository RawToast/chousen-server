$(document).ready(function(){
    registerAttackButtton();
    registerMultiButtons();
    registerCommandButton();
    registerSelfButtons();
    registerCardButtons();
    scrollText();
});

function registerAttackButtton() {
    $('.btn-atk').click(function(evt) {
        evt.preventDefault();

        var id = $(this).attr("id");
        var uri = $(this).attr("uri");


        $.post(uri, JSON.stringify({ targetId: id }), function(result) {
            window.location.replace(window.location.href)
        });
    });
}

function registerMultiButtons() {
    $('.btn-multi').click(function(evt) {
        evt.preventDefault();

        var eids = [];
        var idsString = $(this).attr("eids");
        eids = idsString.split(",");
        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        jQuery.ajax ({
            url: uri,
            type: "POST",
            data: JSON.stringify({ targetId: eids, action: action }),
            dataType: "json",
            contentType: "application/json; charset=utf-8",
            success: function(){
                window.location.replace(window.location.href)
            }
        });
    });
}

function registerCommandButton() {
    $('.btn-com').click(function(evt) {
        evt.preventDefault();

        var id = $(this).attr("eid");
        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        jQuery.ajax ({
            url: uri,
            type: "POST",
            data: JSON.stringify({ targetId: id, action: action }),
            dataType: "json",
            contentType: "application/json; charset=utf-8",
            success: function(){
                window.location.replace(window.location.href)
            }
        });
    });
}

function registerSelfButtons(){
    $('.btn-self').click(function(evt) {
        evt.preventDefault();

        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        jQuery.ajax ({
            url: uri,
            type: "POST",
            data: JSON.stringify({ action: action }),
            dataType: "json",
            contentType: "application/json; charset=utf-8",
            success: function(){
                window.location.replace(window.location.href)
            }
        });
    });
}

function registerCardButtons() {
    $('.btn-card').click(function(evt) {
        evt.preventDefault();

        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        jQuery.ajax ({
            url: uri,
            type: "POST",
            data: JSON.stringify({ action: action }),
            dataType: "json",
            contentType: "application/json; charset=utf-8",
            success: function(){
                window.location.replace(window.location.href)
            }
        });
    });
}

function scrollText() {
    var textarea = $('#gameMessagesText');
    textarea.scrollTop(textarea[0].scrollHeight);
}
