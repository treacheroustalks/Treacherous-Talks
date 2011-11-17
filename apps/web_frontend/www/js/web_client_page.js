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
    page = 'reconfig_game';
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
 * Populate the games_current page
 */
function load_games_current(data) {
    var view_link = function(id) {
        return '<a href="javascript:void(0);" class="btn primary" ' +
               'onclick="get_game_overview(' + id + ')">view</a>';
    }

    // Add view link to all the games
    for ( var i = 0; i < data.length; i++) {
        var obj = data[i];
        data[i]['view'] = view_link(obj.id);
    }

    print(data);

    // Create html table from JSON and display it
    var keys = get_keys(data[0]);
    $('#games_current_data').html(JsonToTable(data, keys, 'gct', 'gcc'));
}

/**
 * Element updates on the page when user logs in
 */
function login_update_elements() {
    $("#login_form").hide();
    $("#logout").show();
    $("#register_menu").hide();
}

/**
 * Element updates on the page when user logs out
 */
function logout_update_elements() {
    $("#logout").hide();
    $("#login_form").show();
    $("#register_menu").show();
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
    default:
        break;
    }
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
 * Validate user update data and if successful, send it to server
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

    if (field == undefined || field == '') {
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
 * Show message in the message box type: success, error, warning message:
 * message to be set in the message box
 */
function set_message(type, message) {
    if (type == undefined || type == "" || message == undefined
            || message == "")
        return false;

    var msgDiv = $('#message');
    var msg = '<div class="alert-message ' + type + '">'
            + '<a class="close" href="javscript:void(0);" '
            + 'onclick="delete_message(); return false;">&times;</a><p>'
            + message + '</p>' + '</div>';
    msgDiv.html(msg).fadeIn('fast');
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