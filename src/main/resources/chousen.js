$(document).ready(function(){
    registerAttackButtton();
    registerBlockButtton();
    registerMultiButtons();
    registerCommandButton();
    registerDropButton();
    registerSelfButtons();
    registerCardButtons();
    registerCampfireButtons();
    registerEquipmentButtons();
    registerCardDropButton();

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

function registerBlockButtton() {
    $('.btn-blk').click(function(evt) {
        evt.preventDefault();

        var uri = $(this).attr("uri");

        $.post(uri, JSON.stringify({}), function(result) {
            window.location.replace(window.location.href)
        });
    });
}

function registerMultiButtons() {
    $('.btn-multi').click(function(evt) {
        evt.preventDefault();

        var idsString = $(this).attr("eids");
        var eids = idsString.split(",");
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

function registerDropButton() {
    $('.btn-drop').click(function(evt) {
        evt.preventDefault();

        var id = $(this).attr("cid");
        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        jQuery.ajax ({
            url: uri,
            type: "POST",
            data: JSON.stringify({ action: action, cardId: id }),
            dataType: "json",
            contentType: "application/json; charset=utf-8",
            success: function(){
                window.location.replace(window.location.href)
            }
        });
    });
}

function registerCardDropButton() {
    $('.btn-dropcard').click(function(evt) {
        evt.preventDefault();

        var id = $(this).attr("cid");
        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        jQuery.ajax ({
            url: uri,
            type: "POST",
            data: JSON.stringify({ action: action, cardId: id }),
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

        sendMessage(uri, action);
    });
}

function registerCardButtons() {
    $('.btn-card').click(function(evt) {
        evt.preventDefault();

        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        sendMessage(uri, action);
    });
}

function registerCampfireButtons() {
    $('.btn-camp').click(function(evt) {
        evt.preventDefault();

        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        sendMessage(uri, action);
    });
}

function registerEquipmentButtons() {
    $('.btn-equip').click(function(evt) {
        evt.preventDefault();

        var cid = $(this).attr("cid");
        var uri = $(this).attr("uri");
        var action = $(this).attr("action");

        jQuery.ajax ({
            url: uri,
            type: "POST",
            data: JSON.stringify({ id: cid, action: action }),
            dataType: "json",
            contentType: "application/json; charset=utf-8",
            success: function(){
                window.location.replace(window.location.href)
            }
        });
    });
}

function sendMessage(uri, action) {
    console.log("Clicked " + action );

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
}

function scrollText() {
    var textarea = $('#gameMessagesText');
    textarea.scrollTop(textarea[0].scrollHeight);
}
