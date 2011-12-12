/**
 * Treacherous Talks web client - main
 *
 * COPYRIGHT
 *
 * Author: Sukumar Yethadka <sukumar@thinkapi.com>
 *
 * Since: 25 Oct 2011 by Bermuda Triangle
 *
 */

/*------------------------------------------------------------------------------
 Site variables
 -----------------------------------------------------------------------------*/
var ws; // Websocket client
var page; // Current page
var delay = 3456; // Delay for message clear (in ms)
var pageDiv = $('#page'); // Div to load pages on
var cookie_name = 'treacherous_talks'; // Cookie name
var cookie_expire_days = 7; // Cookie expiry in days
var initial_page = 'home'; // First page to be loaded
var dash_page = 'dashboard'; // First page to be loaded once logged in
var home_page = initial_page; // Home page. Value changes on user login status
var op_inspect_country; // Which country the operator is inspecting

var debug = true;
var userObj = {
        "id": undefined,
        "email" : undefined,
        "nick" : undefined,
        "name" : undefined,
        "role" : undefined
};

/*------------------------------------------------------------------------------
 jQuery Events
 -----------------------------------------------------------------------------*/
/**
 * Event for handling "Enter" key in forms
 */
$("input").keypress(function(e) {
    if (e.which == 13)
        handle_enter();
});

/*------------------------------------------------------------------------------
 Websocket functions
 -----------------------------------------------------------------------------*/
/**
 * Check if the browser supports websockets
 */
function websocket_check() {
    return window.WebSocket;
}

/**
 * Set up a websocket and connect to the server
 */
function websocket_setup() {
    // Get the endpoint address from the current page
    var ws_url = 'ws://' + window.document.location.host + '/endpoint';
    ws = new WebSocket(ws_url);
    ws.onopen = function() {
        msg_connect_success();
        on_connect();
    };
    ws.onclose = function() {
        msg_connect_close();
    }
    ws.onmessage = function(msg) {
        callback(msg);
    }
}

/*------------------------------------------------------------------------------
 General functions
 -----------------------------------------------------------------------------*/
/**
 * Initialize the web client
 */
function init() {
    // Check if the browser supports websockets
    if (!websocket_check()) {
        msg_unsupported_browser();
        return;
    }

    // Setup a websocket connection
    websocket_setup();
}

/**
 * Init function run once we have a websocket connection
 *
 * @return
 */
function on_connect() {
    // Check cookie
    // If cookie is present -> send the data to the server
    // If cookie is absent -> ask the user to login/register
    var cookie_data = get_cookie();
    if (cookie_data) {
        // Send session id to server and check if it is valid
        page = dash_page;
        var session_id = get_cookie();
        var dataObj = {
            "content" : [ {
                "session_id" : session_id
            } ]
        };
        call_server('get_session_user', dataObj);
    } else {
        page = initial_page;
        load_page();
    }
}

/**
 * Handle messages sent from the server
 */
function callback(msg) {
    print("Server response - Msg -> " + msg.data);
    print("");
    var data = jQuery.parseJSON(msg.data);
    handle_event(data);
}

/**
 * Send messages to the server
 */
function call_server(command, dataObj) {
    print("Server call - Command, DataObject -> " + command + " "
            + JSON.stringify(dataObj));
    print("");
    var msg = {
        "action" : command,
        "data" : dataObj.content
    };
    ws.send(JSON.stringify(msg));
}

/**
 * Logs out the user
 */
function logout() {
    delete_cookie();
    home_page = initial_page;
    logout_update_elements();
    page = home_page;
    load_page();
}

/**
 * Load the local userObj
 *
 * @return
 */
function load_userObj(data) {
    userObj.id = data.id;
    userObj.email = data.email;
    userObj.nick = data.nick;
    userObj.name = data.name;
    userObj.role = data.role;
}

/**
 * clean the local userObj
 *
 * @return
 */
function clean_userObj() {
    userObj.id = undefined;
    userObj.email = undefined;
    userObj.nick = undefined;
    userObj.name = undefined;
    userObj.role = undefined;
}

/*------------------------------------------------------------------------------
 Event handler functions
 -----------------------------------------------------------------------------*/
/**
 * Handles different types of messages sent by the server
 */
function handle_event(data) {
    // Check if a new page has to be loaded
    if (!is_empty_page(data.page)) {
        page = data.page;
        load_page(function() {
            handle_event_page_load(data);
        });
    } else
        handle_event_page_load(data);
}

/**
 * Handle the event and display the message once the page is loaded
 */
function handle_event_page_load(data) {
    // Check if a message has to be displayed
    if (data.message_type != undefined && data.message_value != undefined) {
        set_message(data.message_type, data.message_value);
    }

    // Handle event
    if (data.event != undefined && data.event_data != undefined) {
        event_action(data);
    }
}

/**
 * Take actions based on events sent by the server
 */
function event_action(data) {
    var event = data.event;
    var event_data = data.event_data;

    switch (event) {
    case "in_game_msg_ok":
        onRecvInGameMsg(event_data.game_id, event_data.from_country, event_data.content);
        clear_message();
        break;
    case "off_game_msg_ok":
        onRecvOffGameMsg(event_data.from_nick, event_data.content);
        clear_message();
        break;
    case "user_msg_success":
        onSendMsg(false, "["+userObj.nick+"]");
        clear_message();
        break;
    case "power_msg_success":
    case "game_msg_success":
        onSendMsg(true, "["+userObj.nick+"]");
        clear_message();
        break;
    case "login_success":
        // User is logged in, set the cookie
        set_cookie(event_data.session_id);
        login_update_elements();
        home_page = dash_page;

        // Get the user from the server
        var dataObj = {
            "content" : [ {
                "session_id" : event_data.session_id.toString()
            } ]
        };
        call_server('get_session_user', dataObj);
        break;
    case "login_invalid_data":
        clear_message();
        break;
    case "register_invalid_data":
        clear_message();
        break;
    case "game_msg_invalid_data":
        clear_message();
        break;
    case "get_session_user_success":
        load_userObj(event_data);
        if (is_empty_page(data.page) && page == dash_page) {
            // This case occurs when a logged in user reloads a page
            load_page();
            login_update_elements();
            home_page = dash_page;
        } else
            handle_event_page_load(data);
        clear_message();
        break;
    case "get_session_user_invalid_data":
        logout();
        break;
    case "get_session_user_invalid_session":
        logout();
        break;
    case "update_user_success":
        load_userObj(event_data);
        clear_message();
        break;
    case "update_user_invalid_data":
        clear_message();
        break;
    case "get_game_success":
        if (is_empty_page(data.page) && page == 'reconfig_game') {
            // This case occurs when a game is to be reconfigured
            load_reconfig_game_page(event_data);
        } else
            handle_event_page_load(data);
        break;
    case "get_game_invalid_data":
        if (is_empty_page(data.page) && page == 'reconfig_game') {
            page = home_page;
            load_page();
        }
        clear_message();
        break;
    case "game_overview_success":
        clear_message();
        load_game_overview_data(event_data);
        break;
    case "game_order_success":
        clear_message();
        clear_game_orders();
        break;
    case "games_current_success":
        clear_message();
        load_games_current(event_data);
        break;
    case "game_search_success":
        load_game_search(event_data);
        break;
    case "get_db_stats_success":
        load_get_database_status(event_data);
        break;
    case "operator_game_overview_success":
        load_operator_game_overview(event_data);
        break;
    case "operator_get_game_msg_success":
        load_operator_get_game_msg(event_data);
        break;
    case "get_system_status_success":
        load_get_system_status(event_data);
        break;
    case "get_games_ongoing_success":
        load_get_games_ongoing(event_data);
        break;
    case "stop_game_success":
        clear_message();
        break;
    case "get_presence_success":
        clear_message();
        break;
    case "get_presence_invalid_data":
        clear_message();
    case "mark_as_done_success":
        clear_message();
        break;
    case "get_reports_success":
        load_report_inbox(event_data);
        break;
    case "get_reports_invalid_data":
        clear_message();
        break;
    default:
        break;
    }
}

function is_empty_page(pg) {
    return pg == undefined || pg == "" || pg == page;
}

/*------------------------------------------------------------------------------
 Other functions
 -----------------------------------------------------------------------------*/
function print(msg) {
    if (debug)
        console.log(msg);
}

function benchmarkFunc(somefunc) {
    var start = new Date().getMilliseconds();
    somefunc();
    var stop = new Date().getMilliseconds();
    var executionTime = stop - start;
    console.log("Execution time " + executionTime +
        " milliseconds"); // or alert("Execution time " + executionTime + " milliseconds");
}