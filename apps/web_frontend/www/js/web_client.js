/**
 * Treacherous Talks web client
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
var delay = 1000; // Delay for message clear
var msgDiv = $('#message'); // Div to display messages on
var pageDiv = $('#page'); // Div to load pages on
var cookie_name = 'treacherous_talks'; // Cookie name
var cookie_expire_days = 7; // Cookie expiry in days
var initial_page = 'login'; // First page to be loaded
var dash_page = 'dashboard'; // First page to be loaded once logged in
var debug = true;
var userObj = {
    "email" : undefined,
    "nick" : undefined,
    "fullname" : undefined
};

/*------------------------------------------------------------------------------
 jQuery Events
 -----------------------------------------------------------------------------*/
/**
 * Event for "Enter" key
 */
$(document).keypress(function(e) {
    if (e.keyCode == 13) {
        handle_enter();
    }
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
    print("Server response - Msg");
    print(msg.data);
    var data = jQuery.parseJSON(msg.data);
    handle_event(data);
}

/**
 * Send messages to the server
 */
function call_server(command, dataObj) {
    print("Server call - Command, DataObject");
    print(command);
    print(dataObj);
    var msg = {
        "action" : command,
        "data" : dataObj.content
    };
    ws.send(JSON.stringify(msg));
}

/**
 * Load page on the browser
 */
function load_page(callback) {
    var page_url = window.document.location + 'page/' + page + '.yaws';
    $.get(page_url, function(data) {
        pageDiv.html(data);
        if (callback != undefined)
            callback();
    });
}

/**
 * Load the registration page
 */
function load_register_page() {
    page = 'register';
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

function load_update_user_data() {
    $('#email').val(userObj.email);
    $('#fullname').val(userObj.fullname);
}

function load_userObj(data) {
    userObj.email = data.email;
    userObj.nick = data.nick;
    userObj.fullname = data.fullname;
}

/*------------------------------------------------------------------------------
 Event handler functions
 -----------------------------------------------------------------------------*/
/**
 * Handles different types of messages sent by the server
 */
function handle_event(data) {
    // Check if a new page has to be loaded
    if (data.page != undefined && data.page != "" && page != data.page) {
        page = data.page;
        load_page();
    }

    // Check if a message has to be displayed
    if (data.message_type != undefined && data.message_value != undefined) {
        set_message(data.message_type, data.message_value);
    }

    // Handle event
    if (data.event != undefined && data.event_data != undefined) {
        event_action(data.event, data.event_data);
    }
}

/**
 * Take actions based on events sent by the server
 */
function event_action(event, event_data) {
    switch (event) {
    case "login_success":
        clear_message();
        // User is logged in, set the cookie
        set_cookie(event_data.session_id);
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
    case "get_session_user_invalid_data":
        clear_message();
        break;
    case "get_session_user_success":
        if (page == dash_page) {
            // This case occurs when a logged in user reloads a page

            // Update local userObj
            load_userObj(event_data);
            // Load the dashboard
            load_page();
        }
        break;
    case "update_user_success":
        clear_message();
        load_userObj(event_data);
        break;
    case "update_user_invalid_data":
        clear_message();
        break;
    default:
        break;
    }
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
    case 'login':
        validate_login();
        break;
    case 'register':
        validate_register();
        break;
    case 'update_user':
        validate_update_user();
        break;
    default:
        break;
    }
}

/**
 * Validate user login data and if successful, send it to server
 */
function validate_login() {
    var nick = $('#login_nick').val();
    var password = $('#login_password').val();

    if (check_field_empty(nick, 'nick'))
        return false;
    if (check_field_empty(password, 'password'))
        return false;

    var dataObj = {
        "content" : [ {
            "nick" : nick
        }, {
            "password" : password
        } ]
    };
    call_server('login', dataObj);
}

/**
 * Validate user registration data and if successful, send it to server
 */
function validate_register() {
    var email = $('#reg_email').val();
    var fullname = $('#reg_fullname').val();
    var nick = $('#reg_nick').val();
    var password = $('#reg_password').val();
    var confirm_password = $('#reg_confirm_password').val();

    if (check_field_empty(email, 'email'))
        return false;
    if (check_field_empty(fullname, 'full name'))
        return false;
    if (check_field_empty(nick, 'nick'))
        return false;
    if (check_field_empty(password, 'password'))
        return false;
    if (check_field_empty(confirm_password, 'password confirmation'))
        return false;
    if (check_field_email(email))
        return false;
    if (check_field_password(password, confirm_password))
        return false;

    var dataObj = {
        "content" : [ {
            "email" : email
        }, {
            "fullname" : fullname
        }, {
            "nick" : nick
        }, {
            "password" : password
        } ]
    };
    call_server('register', dataObj);
}

/**
 * Validate user update data and if successful, send it to server
 */
function validate_update_user() {
    var email = $('#email').val();
    var fullname = $('#fullname').val();
    var password = $('#password').val();
    var confirm_password = $('#confirm_password').val();

    if (check_field_empty(email, 'email'))
        return false;
    if (check_field_empty(fullname, 'full name'))
        return false;
    if (check_field_empty(password, 'password'))
        return false;
    if (check_field_empty(confirm_password, 'password confirmation'))
        return false;
    if (check_field_email(email))
        return false;
    if (check_field_password(password, confirm_password))
        return false;

    var dataObj = {
        "content" : [ {
            "session_id" : get_cookie()
        }, {
            "email" : email
        }, {
            "fullname" : fullname
        }, {
            "password" : password
        } ]
    };
    call_server('update_user', dataObj);
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

    if (field == '') {
        set_message('error', msg);
        clear_message();
        return true;
    } else
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
 * Show message in the message box type: success, error, warning message:
 * message to be set in the message box
 */
function set_message(type, message) {
    var msg = '<div class="' + type + '">' + message + '</div>';
    msgDiv.html(msg).fadeIn('fast');
}

/**
 * Clear message div after "delay" time
 *
 * @return
 */
function clear_message() {
    setTimeout(function() {
        msgDiv.html('');
    }, delay);
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
 Other functions
 -----------------------------------------------------------------------------*/
function print(msg) {
    if (debug)
        console.log(msg);
}