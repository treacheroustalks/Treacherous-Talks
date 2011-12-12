/**
 * Treacherous Talks web client - update page content
 *
 * COPYRIGHT
 *
 * Author: Sukumar Yethadka <sukumar@thinkapi.com>
 *
 * Since: 25 Oct 2011 by Bermuda Triangle
 *
 */

/*------------------------------------------------------------------------------
 Page functions
 -----------------------------------------------------------------------------*/

/**
 * Load page on the browser
 */
function load_page(callback) {
    var page_url = window.document.location + 'page/' + page + '.yaws';
    $("#power_msg").val('');
    $.get(page_url, function(data) {
        pageDiv.html(data);
        if (callback != undefined)
            callback();
    });
}

/**
 * Load the server management  page
 */
function get_system_status() {
    page = 'operator';
    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        } ]
    };
    call_server('get_system_status', dataObj);
}

/**
 * Load the system status page
 */
function load_get_system_status(event_data) {
    $('#system_status_data').html('<p>' + nl2br(event_data.system_status) + '</p>');
}

function load_operator_game_overview(event_data) {
    var acc = "";
    var id = event_data.game_id;
    var links = event_data.links;
    for(var i in event_data){
        acc += i + ":&nbsp&nbsp" + "<b>" + event_data[i] + "</b><br>";
    }
    acc += "<br><table><tr><td>";
    if(links)
        acc += '<a href="javascript:void(0);" onclick="operator_get_game_msg(true,\''+id+
                '\',\'\',\'\',\'\');op_inspect_country=false;">(Game context)</a><br>'
    for(var i in links){//year
        var season_phase = links[i];
        acc += '<a id="ys'+ i +'" href="javascript:void(0);" onclick="$(\'#sp'+i+'\').show();\
        $(\'#ys'+i+'\').hide();$(\'#yh'+i+'\').show();">[+] ' + i + '</a>\
        <a id="yh'+ i +'" class="hide" href="javascript:void(0);" onclick="$(\'#sp'+i+'\').hide();\
        $(\'#ys'+i+'\').show();$(\'#yh'+i+'\').hide();">[-] ' + i + '</a>\
        &nbsp&nbsp<a href="javascript:void(0);" onclick="operator_get_game_msg(true,\''+id+
                '\',\''+i+'\',\'\',\'\');op_inspect_country=false;">(context)</a><br>\
        <ul id="sp'+ i +'" class="hide">';

        for(var j in season_phase){
            var countries = season_phase[j];
            acc += '<a id="ys'+ i+j +'" href="javascript:void(0);" onclick="$(\'#sp'+ i+j +'\').show();\
            $(\'#ys'+ i+j +'\').hide();$(\'#yh'+i+j+'\').show();">[+] ' + j + '</a>\
            <a id="yh'+ i+j +'" class="hide" href="javascript:void(0);" onclick="$(\'#sp'+i+j+'\').hide();\
            $(\'#ys'+i+j+'\').show();$(\'#yh'+i+j+'\').hide();">[-] ' + j + '</a>\
            &nbsp&nbsp<a href="javascript:void(0);" onclick="operator_get_game_msg(true,\''+id+
                '\',\''+i+'\',\''+j+'\',\'\');op_inspect_country=false;">(context)</a><br>\
            <ul id="sp'+ i+j +'" class="hide">';

            for(var k=0; k < countries.length; k++){
                acc += '<li><a href="javascript:void(0);" onclick="operator_get_game_msg(false,\''+id+
                '\',\''+i+'\',\''+j+'\',\''+countries[k]+'\');op_inspect_country=\''+countries[k]+'\'">' + countries[k] + '</a></li>';
            }
            acc += "</ul>";
        }
        acc += "</ul>";
    }
    acc += "</td><td><div id='operator_msg_screen'></div></td></tr></table>";
    $('#operator_game_overview').html(acc);
    print(event_data);
}

function load_operator_get_game_msg(event_data){
    var ord = event_data.order;
    var msg = event_data.msg;
    var acc = (ord?"<b>Orders:</b><br>"+nl2br(ord)+"<br><br>":"")+(msg?"<b>Negotiations:</b><br>":"<b>No Message</b>");
    var last_to;
    var bg;
    for(var i=0; i < msg.length; i++){
        var from = msg[i].from;
        var to = msg[i].to;
        if(op_inspect_country){
            if(from == op_inspect_country){
                if(last_to != to)
                    bg = bg!="admin_msg_bg1"?"admin_msg_bg1":"admin_msg_bg2";
                acc += "<div class='"+bg+"'>"+ from +" -> "+ to +": "+ msg[i].content+"</div>";
                last_to = to;
            }else{
                if(last_to != from)
                    bg = bg!="admin_msg_bg1"?"admin_msg_bg1":"admin_msg_bg2";
                acc += "<div class='"+bg+"'>"+ to +" <- "+ from +": "+ msg[i].content+"</div>";
                last_to = from;
            }
        }else
            acc += "<div>"+ from +" -> "+ to +": "+ msg[i].content+"</div>";
    }
    $('#operator_msg_screen').html(acc);
    print(event_data);
}

/**
 * Load the database status page
 */
function load_get_database_status(event_data) {
    var jsonArr = [];
    var dbCount = parseInt(event_data.db_count);
    for(var i = 0; i < dbCount; i++){
        jsonArr.push(jQuery.parseJSON(event_data["db_stats" + i]));
    }
    var keys = get_keys(jsonArr[0]);

    var acc = '<table cellpadding="1" cellspacing="1"><tr><th></th>';
    for(var j=0; j<dbCount; j++){
        acc += "<th><b>Node" + (j+1) + "</b></th>";
    }
    acc += "</tr>";
    for(var i=0; i<keys.length; i++){
        acc += "<tr><td>" + keys[i] + "</td>";
        for(var j=0; j<dbCount; j++){
            acc += '<td height="18"><p id="dbtd"><b>' + jsonArr[j][keys[i]] + "</b></p></td>";
        }
        acc += "</tr>";
    }
    acc += "</table>"
    $('#database_status_data').html('<p>' + acc + '</p>');
}
/**
 * Load the report inbox page
 */
function load_report_inbox(event_data) {
    if(event_data.length > 0){
        var done_link = function(id){
            return '<a href="javascript:void(0);" class="btn primary" ' + 'onclick="mark_as_done(' + id + '); deleteRow(this.parentNode.parentNode.rowIndex, \'rdt\');">Done</a>';
        }

        for(var i = 0; i < event_data.length; i++){
            var obj = event_data[i];
            event_data[i]['Done'] = done_link(obj.id);
        }
        var keys = get_keys(event_data[0]);

        $('#report_data').html(JsonToTable(event_data, keys, 'rdt', 'rdc'));
    }
    else{
        $('#report_data').html('<p>' + "No reports found" + '</p>');
    }
}


/**
 * Load the get ongoing games page for the operator
 */
function load_get_games_ongoing(event_data) {
    var view_link = function(id) {
        return '<a href="javascript:void(0);" class="btn primary" ' + 'onclick="operator_game_overview(' + id + ')">View</a>';
    }

    var stop_link = function(id) {
        return '<a href="javascript:void(0);" class="btn danger" ' + 'onclick="stop_game(' + id + ')">Stop</a>';
    }

    var data = new Array();
    // Add view link to all the games
    for ( var i = 0; i < event_data.length; i++) {
        var obj = new Object();
        obj['Game Id'] = event_data[i];
        obj['View'] = view_link(event_data[i]);
        obj['Stop'] = stop_link(event_data[i]);
        data.push(obj);
    }
    // Create html table from JSON and display it
    var keys = get_keys(data[0]);

    $('#games_ongoing_data').html(JsonToTable(data, keys, 'gnt', 'gnc'));
}


/**
 * Load a page for setting moderator
 */
function load_add_remove_moderator_page() {
    page = 'add_remove_moderator';
    load_page();
}

/**
 * Load the operator control pannel page
 */
function load_operator_page() {
    page = 'operator';
    load_page();
}

/**
 * Load the operator control pannel page
 */
function load_moderator_page() {
    page = 'moderator';
    load_page();
}

/**
 * Load the registration page
 */
function load_register_page() {
    page = 'register';
    load_page();
}

/**
 * Load the registration page
 */
function load_home_page() {
    page = home_page;
    load_page();
}

/**
 * Load the login page
 */
function load_login_page() {
    page = 'login';
    load_page();
}

/**
 * Load the update_user page
 */
function load_update_user_page() {
    page = 'update_user';
    load_page(load_update_user_data);
}

/**
 * Update the user data on update_user page
 *
 * @return
 */
function load_update_user_data() {
    $('#email').val(userObj.email);
    $('#name').val(userObj.name);
}

/**
 * Load the reconfig_game page
 */
function load_reconfig_game_page(page_data) {
    load_page(function() {
        load_reconfig_game_data(page_data)
    });
}

/**
 * Update the game overview page with event data
 */
function load_game_overview_data(page_data) {
    if (page_data.game_status == 'ongoing'){
        $('#game_id').val(page_data.game_id);
        $('#game_info').html(nl2br(page_data.game_info));
        $('#game').html(nl2br(page_data.game));
        $('#provinces').html(nl2br(page_data.provinces));
        $('#units').html(nl2br(page_data.units));
        $('#orders').html(nl2br(page_data.orders));
    }
    else{
        $('#game_info').html(nl2br(page_data.game_info));
        $('#game').html(nl2br(page_data.game));
        $('#players').html(nl2br(page_data.players));
        $('#map').html(nl2br(page_data.map));
        $('#orders').html(nl2br(page_data.orders));
    }
}

/**
 * Update the game data on reconfig_game page
 *
 * @return
 */
function load_reconfig_game_data(page_data) {
    $('#game_legend').append(page_data.id);
    $('#game_id').val(page_data.id);
    $('#name').val(page_data.name);
    $('#description').val(page_data.description);
    $('#pass').val(page_data.password);
    $('#press').val(page_data.press);
    $('#order_phase').val(page_data.order_phase);
    $('#retreat_phase').val(page_data.retreat_phase);
    $('#build_phase').val(page_data.build_phase);
    $('#waiting_time').val(page_data.waiting_time);
    $('#num_players').val(page_data.num_players);
}

/**
 * Load the create_game page
 */
function load_create_game_page() {
    page = 'create_game';
    load_page();
}

/**
 * Load the game_search page
 */
function load_game_search_page() {
    page = 'game_search';
    load_page();
}

/**
 * Populate the games_current page
 */
function load_games_current(data) {
    var view_link = function(id) {
        return '<a href="javascript:void(0);" class="btn primary" ' + 'onclick="get_game_overview(' + id + ')">View</a>';
    }
    var reconfig_link = function(id) {
        return '<a href="javascript:void(0);" class="btn primary" ' + 'onclick="reconfig_game(' + id + ')">Reconfig</a>';
    }

    // Add view link to all the games
    for ( var i = 0; i < data.length; i++) {
        var obj = data[i];
        data[i]['View'] = view_link(obj.id);
        if(userObj.id == obj.creator_id)
            data[i]['Reconfig'] = reconfig_link(obj.id);
        else
            data[i]['Reconfig'] = "-";

        // Filter out some fields
        delete data[i]['creator_id'];
        delete data[i]['order_phase'];
        delete data[i]['retreat_phase'];
        delete data[i]['build_phase'];
        delete data[i]['num_players'];
        delete data[i]['waiting_time'];
    }

    // Create html table from JSON and display it
    var keys = get_keys(data[0]);
    $('#games_current_data').html(JsonToTable(data, keys, 'gct', 'gcc'));
}

/**
 * Populate the game_search page
 */
function load_game_search(data) {
    // Create html table from JSON and display it
    var keys = get_keys(data[0]);
    $('#game_search_data').html('<h2>Game search results</h2>');
    $('#game_search_data').append(JsonToTable(data, keys, 'gst', 'gsc'));
}

/**
 * Element updates on the page when user logs in
 */
function login_update_elements() {
    $("#login_form").hide();
    $("#logout").show();
    $("#register_menu").hide();
    $("#user_nick").html("<b>"+userObj.nick+"</b>");
    drawChatBoxes();
    if(userObj.role!="user")
        $("#power_section").show();
    else
        $("#power_section").hide();
    if(userObj.role == "moderator"){
        $("#moderator_menu").show();
    }
    if(userObj.role == "operator"){
        $("#operator_menu").show();
    }
}

/**
 * Element updates on the page when user logs out
 */
function logout_update_elements() {
    clean_userObj();
    $("#moderator_menu").hide();
    $("#operator_menu").hide();
    $("#logout").hide();
    $("#login_form").show();
    $("#register_menu").show();
    cleanChatBoxes();
}

/*------------------------------------------------------------------------------
 Page functions
 -----------------------------------------------------------------------------*/

/**
 * Call server for for "games current"
 */
function get_games_current() {
    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        } ]
    };
    call_server('games_current', dataObj);
}

/**
 * Refresh the game overview page
 */
function refresh_game() {
    var form = get_form_data('#play_game_form');
    get_game_overview(form.game_id);
}


/**
 * Call server to get game overview of specified game
 */
function get_game_overview(game_id) {
    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "game_id" : game_id
        } ]
    };
    call_server('game_overview', dataObj);
}

/**
 * Call server to reconfigure a game
 */
function reconfig_game(game_id) {
    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "game_id" : game_id
        } ]
    };
    page = 'reconfig_game';
    call_server('get_game', dataObj);
}

/**
 * Call server to stop given game
 */
function stop_game(game_id) {
    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "game_id" : game_id
        } ]
    };
    call_server('stop_game', dataObj);
}

/**
  * Call server to get reports
  */
function get_reports() {
    var dataObj = {
        "content" : [
            { "session_id" : get_cookie() }
        ]
    };
    call_server('get_reports', dataObj);
}

/**
  * Call server to mark an issue as done
  */
function mark_as_done(issue_id){
    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "issue_id" : issue_id
        } ]
    };
    call_server('mark_as_done', dataObj);
}


/**
 * Call server to send off game
 */
function send_off_game_message() {
    var chat_msg = $('#chat_msg').val();
    var chat_to = $('#chat_to').val();
    if (check_field_empty(chat_msg, 'message, cannot send empty message'))
        return false;
    if (check_field_empty(chat_to, 'recipient, no recipient specified'))
        return false;
    var dataObj = {
        "content" : [
            { "session_id" : get_cookie() },
            { "to" : chat_to },
            { "content" : chat_msg }
        ]
    };
    call_server('user_msg', dataObj);
}

/**
 * Call server to send in game message
 */
function send_in_game_message() {
    var press_game_id = $('#press_game_id').val();
    var press_msg = $('#press_msg').val();
    if (check_field_empty(press_game_id, 'Press Game Id'))
        return false;
    if (check_field_int(press_game_id, 'Press Game Id'))
        return false;
    if (check_field_empty(press_msg, 'Cannot send empty message'))
        return false;
    var dataObj = {
        "content" : [
            { "session_id" : get_cookie() },
            { "game_id" : press_game_id },
            { "to" : $('#press_to').val() },
            { "content" : press_msg }
        ]
    };
    if($("#power_msg").prop('checked'))
        call_server('power_msg', dataObj);
    else
        call_server('game_msg', dataObj);
}

function get_database_status() {
    var dataObj = {
        "content" : [
            { "session_id" : get_cookie() }
        ]
    };
    call_server('get_db_stats', dataObj);
}

function get_games_ongoing() {
    var dataObj = {
        "content" : [
            { "session_id" : get_cookie() }
        ]
    };
    call_server('get_games_ongoing', dataObj);
}

function operator_game_overview(inspect_game_id) {
    var dataObj = {
        "content" : [
            { "game_id" : inspect_game_id },
            { "session_id" : get_cookie() }
        ]
    };
    call_server('operator_game_overview', dataObj);
}

function operator_get_game_msg(msg_only, game_id, year, season_phase, country) {
    var order_key = msg_only?"":game_id+"-"+year+"-"+season_phase+"-"+country;
    var query = "game_id="+game_id;
    if(year)
        query += " AND year="+year;
    if(season_phase){
        var sp = season_phase.split("-");
        var season = sp[0];
        var phase = sp[1];
        query += " AND season="+season+" AND phase="+phase;
    }
    if(country)
        query += " AND (from_country="+country+" OR "+"to_country="+country+")";

    var dataObj = {
        "content" : [
            { "order_key" : order_key },
            { "query" : query },
            { "session_id" : get_cookie() }
        ]
    };
    call_server('operator_get_game_msg', dataObj);
}

/*------------------------------------------------------------------------------
 Form cleanup functions
 -----------------------------------------------------------------------------*/
function clear_game_orders() {
    $("#game_order").val("");
}

/*------------------------------------------------------------------------------
 Validation functions
 -----------------------------------------------------------------------------*/
/**
 * Validate form data
 */
function validate() {
    handle_enter();
}

/**
 * Handles all events when the Enter key is pressed
 */
function handle_enter() {
    switch (page) {
    case 'register':
        validate_register();
        break;
    case 'update_user':
        validate_update_user();
        break;
    case 'create_game':
        validate_game_data();
        break;
    case 'reconfig_game':
        validate_game_data();
        break;
    case 'game':
        validate_game();
        break;
    case 'game_search':
        validate_game_search();
        break;
    case 'add_remove_moderator':
        validate_add_remove_moderator();
        break;
    default:
        break;
    }
}

function  validate_add_remove_moderator() {
    var form = get_form_data('#add_remove_moderator_form');
    var moderator = $('#is_moderator').prop('checked')?"add":"remove";
    if (check_field_empty(form.nick, 'nick'))
        return false;

    var dataObj = {
        "content" : [{
            "session_id" : get_cookie()
        }, {
            "nick" : form.nick
        }, {
            "is_moderator" : moderator
        } ]
    };
    call_server('assign_moderator', dataObj);
}
/**
 * Validate user login data and if successful, send it to server
 */
function validate_login() {
    var form = get_form_data('#login_form');

    if (check_field_empty(form.nick, 'nick'))
        return false;
    if (check_field_empty(form.password, 'password'))
        return false;

    var dataObj = {
        "content" : [ {
            "nick" : form.nick
        }, {
            "password" : form.password
        } ]
    };
    call_server('login', dataObj);
}

/**
 * Validate temporary form on dashboard to get game_id
 * DEPRECATED
 */
function validate_dashboard_reconfig() {
    var form = get_form_data('#reconfig_form');

    if (check_field_empty(form.game_id, 'Game Id'))
        return false;

    if (check_field_int(form.game_id, 'Game Id'))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "game_id" : form.game_id
        } ]
    };
    call_server('get_game', dataObj);
}

/**
 * Validate temporary form on dashboard to get user presence
 */
function validate_dashboard_presence() {
    var form = get_form_data('#check_presence_form');

    if (check_field_empty(form.user_nick, 'User nick'))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "nick" : form.user_nick
        } ]
    };
    call_server('get_presence', dataObj);
}

/**
 * Validate temporary form on dashboard to join a game
 */
function validate_dashboard_join() {
    var form = get_form_data('#join_game_form');

    if (check_field_empty(form.game_id, 'Game Id'))
        return false;
    if (check_field_empty(form.country, 'Country'))
        return false;

    if (check_field_int(form.game_id, 'Game Id'))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "game_id" : form.game_id
        }, {
            "country" : form.country
        } ]
    };
    call_server('join_game', dataObj);
}

/**
 * Validate temporary form on dashboard to get overview of a game
 * DEPRECATED
 */
function validate_dashboard_overview() {
    var form = get_form_data('#game_overview_form');

    if (check_field_empty(form.game_id, 'Game Id'))
        return false;

    if (check_field_int(form.game_id, 'Game Id'))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "game_id" : form.game_id
        } ]
    };
    call_server('game_overview', dataObj);
}

/**
 * Validate user registration data and if successful, send it to server
 */
function validate_register() {
    var form = get_form_data('#register_form');

    if (check_field_empty(form.email, 'email'))
        return false;
    if (check_field_empty(form.name, 'full name'))
        return false;
    if (check_field_empty(form.nick, 'nick'))
        return false;
    if (check_field_empty(form.password, 'password'))
        return false;
    if (check_field_empty(form.confirm_password, 'password confirmation'))
        return false;
    if (check_field_email(form.email))
        return false;
    if (check_field_password(form.password, form.confirm_password))
        return false;

    var dataObj = {
        "content" : [ {
            "email" : form.email
        }, {
            "name" : form.name
        }, {
            "nick" : form.nick
        }, {
            "password" : form.password
        } ]
    };
    call_server('register', dataObj);
}

/**
 * Validate user update data and if successful, send it to server
 */
function validate_update_user() {
    var form = get_form_data('#update_user_form');

    if (check_field_empty(form.email, 'email'))
        return false;
    if (check_field_empty(form.name, 'full name'))
        return false;
    if (check_field_empty(form.password, 'password'))
        return false;
    if (check_field_empty(form.confirm_password, 'password confirmation'))
        return false;
    if (check_field_email(form.email))
        return false;
    if (check_field_password(form.password, form.confirm_password))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "email" : form.email
        }, {
            "name" : form.name
        }, {
            "password" : form.password
        } ]
    };
    call_server('update_user', dataObj);
}

/**
 * Validate game data and if successful, send it to server
 */
function validate_game_data() {
    switch (page) {
    case 'reconfig_game':
        var form = get_form_data('#reconfig_game_form');
        break;
    case 'create_game':
        var form = get_form_data('#create_game_form');
        break;
    default:
        return false;
    }

    if (check_field_empty(form.name, 'name'))
        return false;
    if (check_field_empty(form.press, 'press'))
        return false;
    if (check_field_empty(form.order_phase, 'order_phase'))
        return false;
    if (check_field_empty(form.retreat_phase, 'retreat_phase'))
        return false;
    if (check_field_empty(form.build_phase, 'build_phase'))
        return false;
    if (check_field_empty(form.waiting_time, 'waiting_time'))
        return false;
    if (check_field_empty(form.num_players, 'num_players'))
        return false;

    if (check_field_int(form.order_phase, 'order_phase'))
        return false;
    if (check_field_int(form.retreat_phase, 'retreat_phase'))
        return false;
    if (check_field_int(form.build_phase, 'build_phase'))
        return false;
    if (check_field_int(form.waiting_time, 'waiting_time'))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "name" : form.name
        }, {
            "description" : form.description
        }, {
            "password" : form.password
        }, {
            "press" : form.press
        }, {
            "order_phase" : form.order_phase
        }, {
            "retreat_phase" : form.retreat_phase
        }, {
            "build_phase" : form.build_phase
        }, {
            "waiting_time" : form.waiting_time
        }, {
            "num_players" : form.num_players
        } ]
    };

    switch (page) {
    case 'reconfig_game':
        dataObj.content.push( {
            "game_id" : form.game_id
        });
        call_server('reconfig_game', dataObj);
        break;
    case 'create_game':
        call_server('create_game', dataObj);
        break;
    default:
        return false;
    }
}

/**
 * Validate game search data and if successful, send it to server
 */
function validate_game_search() {
    var form = get_form_data('#search_game_form');

    if (!is_field_empty(form.order_phase)
            && check_field_int(form.order_phase, 'order_phase'))
        return false;
    if (!is_field_empty(form.retreat_phase)
            && check_field_int(form.retreat_phase, 'retreat_phase'))
        return false;
    if (!is_field_empty(form.build_phase)
            && check_field_int(form.build_phase, 'build_phase'))
        return false;
    if (!is_field_empty(form.waiting_time)
            && check_field_int(form.waiting_time, 'waiting_time'))
        return false;

    // Check if all the form fields are empty
    var is_empty = true;
    $.each(form, function(key, val) {
        if (!is_field_empty(val))
            is_empty = false;
    });
    if (is_empty) {
        set_message('error', 'Please enter some search criteria.');
        clear_message();
        return false;
    }

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "name" : form.name
        }, {
            "description" : form.description
        }, {
            "status" : form.status
        }, {
            "press" : form.press
        }, {
            "order_phase" : form.order_phase
        }, {
            "retreat_phase" : form.retreat_phase
        }, {
            "build_phase" : form.build_phase
        }, {
            "waiting_time" : form.waiting_time
        }, {
            "num_players" : form.num_players
        } ]
    };

    call_server('game_search', dataObj);
}

function validate_game() {
    var form = get_form_data('#play_game_form');

    if (check_field_empty(form.game_order, 'game_order'))
        return false;
    if (check_field_empty(form.game_id, 'game_id'))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "game_id" : form.game_id
        }, {
            "game_order" : form.game_order
        } ]
    };

    call_server('game_order', dataObj);
}

/**
 * Check if a given field is empty and display appropriate message
 *
 * @param field
 * @param name
 * @param message
 * @return Boolean
 */
function check_field_empty(field, name, message) {
    if (message != undefined)
        var msg = message;
    else
        var msg = 'Please enter ' + name;

    if (is_field_empty(field)) {
        set_message('error', msg);
        clear_message();
        return true;
    } else
        return false;
}

/**
 * Check if a given field is empty
 */
function is_field_empty(field) {
    if (field == undefined || field == '')
        return true;
    else
        return false;
}

/**
 * Check is the given email is valid
 *
 * @param email
 * @return Boolean
 */
function check_field_email(email) {
    var regex = /^([a-zA-Z0-9_\.\-\+])+\@(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,4})+$/;
    if (!regex.test(email)) {
        set_message('error', 'Invalid email. Please enter a valid email.');
        clear_message();
        return true;
    }
    return false;
}

/**
 * Check if passwords match
 *
 * @param password
 * @param confirm_password
 * @return Boolean
 */
function check_field_password(password, confirm_password) {
    if (password != confirm_password) {
        set_message('error', 'Passwords don\'t match. Please try again.');
        clear_message();
        return true;
    }
    return false;
}

/**
 * Check if the given field is an integer
 */
function check_field_int(field, name) {
    if ((parseFloat(field) != parseInt(field))) {
        set_message('error', name + ' should be an integer.');
        clear_message();
        return true;
    } else
        return false;
}

/**
 * Gets all the form data
 */
function get_form_data(form) {
    var values = {};
    $.each($(form).serializeArray(), function(i, field) {
        values[field.name] = field.value;
    });
    return values;
}

/*------------------------------------------------------------------------------
 Cookie functions
 -----------------------------------------------------------------------------*/
/**
 * Get cookie from the browser
 */
function get_cookie() {
    var cookie = $.cookie(cookie_name);
    if (cookie == null)
        return false;
    else
        return $.cookie(cookie_name);
}

/**
 * Set cookie in the browser
 */
function set_cookie(cookie_data) {
    $.cookie(cookie_name, cookie_data, {
        expires : cookie_expire_days,
        path : '/'
    });
}

/**
 * Remove cookie from the browser
 */
function delete_cookie() {
    $.cookie(cookie_name, null);
}

/*------------------------------------------------------------------------------
 Message functions
 -----------------------------------------------------------------------------*/
/**
 * Show message to user: success, error, warning, chat message:
 * message to be set in the message box / the chat box.
 */
function set_message(type, message) {
    if (type == undefined || type == "" || message == undefined
            || message == "")
        return false;

    var in_game = false;
    switch (type) {
    case 'invisible':
        print(message);
        break;
    case 'success':
    case 'error':
    case 'warning':
        var msgDiv = $('#message');
        var msg = '<div class="alert-message ' + type + '">'
            + '<a class="close" href="javscript:void(0);" '
            + 'onclick="delete_message(); return false;">&times;</a><p>'
            + message + '</p>' + '</div>';
        msgDiv.html(msg).fadeIn('fast');
        break;
    default:
        print('web_client_page.js, set_message: message "'
              + message + '" of type "'
              + type + '" could not be handled');
    }
}

/**
 * Clear message div after "delay" time
 *
 * @return
 */
function clear_message() {
    var msgDiv = $('#message');
    setTimeout(function() {
        msgDiv.html('');
    }, delay);
}

function delete_message() {
    var msgDiv = $('#message');
    msgDiv.html('');
}

function msg_unsupported_browser() {
    set_message('error', 'Unsupported browser!');
}

function msg_connect_success() {
    set_message('success', 'Connected to server via websockets');
    clear_message();
}

function msg_connect_close() {
    set_message('error', 'Connection to server closed');
}

/*------------------------------------------------------------------------------
 General functions
 -----------------------------------------------------------------------------*/

function nl2br(str, is_xhtml) {
    var breakTag = (is_xhtml || typeof is_xhtml === 'undefined') ? '<br />'
            : '<br>';
    return (str + '').replace(/([^>\r\n]?)(\r\n|\n\r|\r|\n)/g,
            '$1' + breakTag + '$2');
}

function get_keys(obj) {
    var keys = [];
    for ( var key in obj) {
        keys.push(key);
    }
    return keys;
}

function deleteRow(row, table){
    document.getElementById(table).deleteRow(row);
}
