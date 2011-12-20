/*

Copyright (c) 2009 Anant Garg (anantgarg.com | inscripts.com)

This script may be used for non-commercial purposes only. For any
commercial purposes, please contact the author at
anant.garg@inscripts.com

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

*/

var windowFocus = true;
var username;
var originalTitle;
var chatboxFocus = new Array();
var newMessages = new Array();
var newMessagesWin = new Array();
var chatBoxes = new Array();
var temp;
$(document).ready(function(){
    originalTitle = document.title;

    $([window, document]).blur(function(){
        windowFocus = false;
    }).focus(function(){
        windowFocus = true;
        document.title = originalTitle;
    });
});

function restructureChatBoxes() {
    align = 0;
    for (x in chatBoxes) {
        chatboxtitle = chatBoxes[x];

        if ($("#chatbox_"+chatboxtitle).css('display') != 'none') {
            if (align == 0) {
                $("#chatbox_"+chatboxtitle).css('right', '20px');
            } else {
                width = (align)*(225+7)+20;
                $("#chatbox_"+chatboxtitle).css('right', width+'px');
            }
            align++;
        }
    }
}


function drawChatBoxes() {
    var offGameBoxTitle='off_game';
    var offGameHtml ='\
    <div class="chatboxhead">\
        <div class="chatboxoptions">\
            <a href="javascript:void(0)" onclick="javascript:toggleChatBoxGrowth(\''+offGameBoxTitle+'\')">Off Game Message</a>\
        </div><br clear="all"/>\
    </div>\
    <div class="chatboxcontent"></div>\
    <div class="chatboxinput">\
        To: <input type="text" id="chat_to"/>\
        <textarea id="chat_msg" class="chatboxtextarea" onkeydown="javascript:return checkChatBoxInputKey(event,this,\''+offGameBoxTitle+'\');"></textarea>\
    </div>';

    var inGameBoxTitle='in_game';
    var inGameHtml ='\
    <div class="chatboxhead">\
        <div class="chatboxoptions">\
            <a href="javascript:void(0)" onclick="javascript:toggleChatBoxGrowth(\''+inGameBoxTitle+'\')">In Game Message</a>\
        </div><br clear="all"/>\
    </div>\
    <div class="chatboxcontent"></div>\
    <div class="chatboxinput">\
        GameId:<input type="text" id="press_game_id"/>\
        <select id="press_to" multiple="multiple">\
                        <option value="england">England</option>\
                        <option value="germany">Germany</option>\
                        <option value="france">France</option>\
                        <option value="austria">Austria</option>\
                        <option value="italy">Italy</option>\
                        <option value="russia">Russia</option>\
                        <option value="turkey">Turkey</option>\
        </select>\
        <div id="power_section">\
            <b>Admin privilege:</b><input type="checkbox" id="power_msg"/>\
        </div>\
        <textarea id="press_msg" class="chatboxtextarea" onkeydown="javascript:return checkChatBoxInputKey(event,this,\''+inGameBoxTitle+'\');"></textarea>\
    </div>';
    createChatBox(inGameBoxTitle, inGameHtml);
    createChatBox(offGameBoxTitle, offGameHtml);
    //$("#chatbox_"+chatuser+" .chatboxtextarea").focus();
}

function cleanChatBoxes(){
    $('.chatboxcontent').html('');
    $('#power_msg').prop('checked','');
    $('#press_to').val('');
    $('#press_game_id').val('');
    $('#chat_to').val('');
    $('.chatboxtextarea').val('');
    closeChatBox('off_game');
    closeChatBox('in_game');
}

function createChatBox(chatboxtitle,htmlCode) {
    if ($("#chatbox_"+chatboxtitle).length > 0) {
        if ($("#chatbox_"+chatboxtitle).css('display') == 'none') {
            $("#chatbox_"+chatboxtitle).css('display','block');
            restructureChatBoxes();
        }
        $("#chatbox_"+chatboxtitle+" .chatboxtextarea").focus();
        return;
    }

    $(" <div />" ).attr("id","chatbox_"+chatboxtitle)
    .addClass("chatbox")
    .html(htmlCode)
    .appendTo($( "body" ));
    $("#chatbox_"+chatboxtitle).css('bottom', '0px');

    chatBoxeslength = 0;

    for (x in chatBoxes) {
        if ($("#chatbox_"+chatBoxes[x]).css('display') != 'none') {
            chatBoxeslength++;
        }
    }

    if (chatBoxeslength == 0) {
        $("#chatbox_"+chatboxtitle).css('right', '20px');
    } else {
        width = (chatBoxeslength)*(225+7)+20;
        $("#chatbox_"+chatboxtitle).css('right', width+'px');
    }

    chatBoxes.push(chatboxtitle);

    chatboxFocus[chatboxtitle] = false;

    $("#chatbox_"+chatboxtitle+" .chatboxtextarea").blur(function(){
        chatboxFocus[chatboxtitle] = false;
        $("#chatbox_"+chatboxtitle+" .chatboxtextarea").removeClass('chatboxtextareaselected');
    }).focus(function(){
        chatboxFocus[chatboxtitle] = true;
        newMessages[chatboxtitle] = false;
        $('#chatbox_'+chatboxtitle+' .chatboxhead').removeClass('chatboxblink');
        $("#chatbox_"+chatboxtitle+" .chatboxtextarea").addClass('chatboxtextareaselected');
    });

    $("#chatbox_"+chatboxtitle).show();
}

function chatBoxBlink(title){
    var t='#chatbox_'+title+' .chatboxhead';
    var intv = setInterval("chatBoxBlink2('"+t+"')", 1000);
    setTimeout("clearInterval("+intv+")", 6800);
}
function chatBoxBlink2(title){
    $(title).toggleClass('chatboxblink');
}

function closeChatBox(chatboxtitle) {
    $('#chatbox_'+chatboxtitle).css('display','none');
    restructureChatBoxes();
}

function toggleChatBoxGrowth(chatboxtitle) {
    if ($('#chatbox_'+chatboxtitle+' .chatboxcontent').css('display') == 'none') {

        var minimizedChatBoxes = new Array();

        if ($.cookie('chatbox_minimized')) {
            minimizedChatBoxes = $.cookie('chatbox_minimized').split(/\|/);
        }

        var newCookie = '';

        for (i=0;i<minimizedChatBoxes.length;i++) {
            if (minimizedChatBoxes[i] != chatboxtitle) {
                newCookie += chatboxtitle+'|';
            }
        }

        newCookie = newCookie.slice(0, -1);
        $.cookie('chatbox_minimized', newCookie);
        $('#chatbox_'+chatboxtitle+' .chatboxcontent').css('display','block');
        $('#chatbox_'+chatboxtitle+' .chatboxinput').css('display','block');
        $("#chatbox_"+chatboxtitle+" .chatboxcontent").scrollTop($("#chatbox_"+chatboxtitle+" .chatboxcontent")[0].scrollHeight);
        $("#chatbox_"+chatboxtitle+" .chatboxtextarea").focus();
    } else {

        var newCookie = chatboxtitle;

        if ($.cookie('chatbox_minimized')) {
            newCookie += '|'+$.cookie('chatbox_minimized');
        }


        $.cookie('chatbox_minimized',newCookie);
        $('#chatbox_'+chatboxtitle+' .chatboxcontent').css('display','none');
        $('#chatbox_'+chatboxtitle+' .chatboxinput').css('display','none');
    }

}

function checkChatBoxInputKey(event,chatboxtextarea,chatboxtitle) {

    if(event.keyCode == 13 && event.shiftKey == 0)  {
        message = $(chatboxtextarea).val();
        message = message.replace(/^\s+|\s+$/g,"");

        if (message != '') {
            if(chatboxtitle=="in_game"){
                send_in_game_message();
            }else if(chatboxtitle=="off_game"){
                send_off_game_message();
            }
        }

        return false;
    }

    var adjustedHeight = chatboxtextarea.clientHeight;
    var maxHeight = 94;

    if (maxHeight > adjustedHeight) {
        adjustedHeight = Math.max(chatboxtextarea.scrollHeight, adjustedHeight);
        if (maxHeight)
            adjustedHeight = Math.min(maxHeight, adjustedHeight);
        if (adjustedHeight > chatboxtextarea.clientHeight)
            $(chatboxtextarea).css('height',adjustedHeight+8 +'px');
    } else {
        $(chatboxtextarea).css('overflow','auto');
    }

}

function updateOffGameChatBox(title, who, msg){
    var msg_div = $('<div/>', {class: "chatboxmessage" });
    var from_span = $('<span/>', {text: who+":  ", class: "chatboxmessagefrom" });
    var msg_span = $('<span/>', {text: msg, class: "chatboxmessagecontent" });
    var msg_format = msg_div.append(from_span, msg_span);
    $("#chatbox_"+title+" .chatboxcontent").append(msg_format);
    for (var i=0;i<chatBoxes.length;i++) {
            temp = chatBoxes[i];
            $("#chatbox_"+temp+" .chatboxcontent").scrollTop($("#chatbox_"+temp+" .chatboxcontent")[0].scrollHeight);
    }
}

function updateInGameChatBox(title, game_id, who, msg){
    var msg_div = $('<div/>', {class: "chatboxmessage" });
    game_id = '<a href="javascript:void(0)" onclick="get_game_overview('+game_id+')">'+game_id+'</a>';
    var from_span = $('<span/>', {html: who+"@"+game_id+":", class: "chatboxmessagefrom" });
    var msg_span = $('<span/>', {text: msg, class: "chatboxmessagecontent" });
    var msg_format = msg_div.append(from_span, msg_span);
    $("#chatbox_"+title+" .chatboxcontent").append(msg_format);
    for (var i=0;i<chatBoxes.length;i++) {
            temp = chatBoxes[i];
            $("#chatbox_"+temp+" .chatboxcontent").scrollTop($("#chatbox_"+temp+" .chatboxcontent")[0].scrollHeight);
    }
}

function onRecvOffGameMsg(from, msg){
    updateOffGameChatBox("off_game", from, msg);
    chatBoxBlink("off_game");
}

function onRecvInGameMsg(game_id, from, msg){
    updateInGameChatBox("in_game", game_id, from, msg);
    chatBoxBlink("in_game");
}

function onSendMsg(inGame){
    var chatArea;
    var chatBox;
    var msg_to;
    var gid = $('#press_game_id').val();
    if(inGame){
        var arr=$('#press_to').val();
        if(arr.length == 1){
            msg_to = arr[0]+"@";
        }else if(arr && arr.length < 7){
            for(var i=0; i<arr.length; i++){
                arr[i]=arr[i].substr(0,2);
            }
            msg_to = "["+arr.join()+"]@";
        }else{
            msg_to = "all@";
        }
        msg_to += '<a href="javascript:void(0)" onclick="get_game_overview('+gid+')">'+gid+'</a>';
        chatArea = $('#press_msg');
        chatBox = "in_game";
    }else{
        chatArea = $('#chat_msg');
        chatBox = "off_game";
        msg_to = $('#chat_to').val();
    }
    var msg_div = $('<div/>', {class: "chatboxmessage" });
    var to_span = $('<span/>', {html: "<font color='#229922'>To "+msg_to+":</font>  ", class: "chatboxmessagefrom" });
    var msg_span = $('<span/>', {text: chatArea.val(), class: "chatboxmessagecontent" });
    var msg_format = msg_div.append(to_span, msg_span);
    $("#chatbox_"+chatBox+" .chatboxcontent").append(msg_format);
    chatArea.val('');
    chatArea.focus();
    chatArea.css('height','44px');
}

function systemChatInfo(title, msg){
    $("#chatbox_"+title+" .chatboxcontent").append('<div class="chatboxmessage"><span class="chatboxinfo">'+msg+'</span></div>');
}
