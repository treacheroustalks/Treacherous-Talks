/**
 * Treacherous Talks web client - main
 *
 * COPYRIGHT
 *
 * Author: Sukumar Yethadka <sukumar@thinkapi.com>
 *
 * Since: 28 Nov 2011 by Bermuda Triangle
 *
 */

/* Global variables */
var debug = true;
var time_interval = 60;
var vars = ['successful', 'failed'];
var content_file = 'content.json';
var report_file = 'report.json';
var content;
var report;
var graph_names = new Object();

/*------------------------------------------------------------------------------
Load functions
-----------------------------------------------------------------------------*/

/**
 * Initialize
 */
function init() {
    // Load json data
    content = $.parseJSON(get(content_file));
    report = $.parseJSON(get(report_file));
    create_tabs();
    load_names();
    load_tab('home');
}

/**
 * Load the names of the reports locally for display purposes
 */
function load_names() {
    var home_data = content.home;
    $.each(home_data, function(key, value) {
        var section = Object.keys(value)[0];
        $.each(content[section], function(k, v) {
            var ky = Object.keys(v)[0];
            graph_names[ky] = v[ky];
        })
    })
}

/*------------------------------------------------------------------------------
 Page functions
 -----------------------------------------------------------------------------*/

/**
 * Load a tab. Currently specialized only to work with the home page
 */
function load_tab(tab_name) {
    // Its just home page for now. Hide general graphs and show home page graphs
    $('#graph').hide();
    $('.home_graph').show();

    // Riak tests
    home_graph('riak_tests', 'graph_riak', 'Riak configuration tests', vars);
    home_graph('worker_tests_gen_all', 'graph_gen_all', 'General flow - All workers', ['successful']);
    home_graph('worker_tests_gen_message', 'graph_gen_message', 'General flow - Message workers', ['successful']);
    home_graph('worker_tests_gen_controller', 'graph_gen_controller', 'General flow - Controller workers', ['successful']);
    home_graph('worker_tests_gen_game', 'graph_gen_game', 'General flow - Game workers', ['successful']);
    home_graph('worker_tests_gen_db', 'graph_gen_db', 'General flow - DB workers', ['successful']);
}

/**
 * Print the graphs on the home page
 */
function home_graph(graphs_key, graph_div, graph_name, vars_arr) {
    var files = new Array();
    $.map(content[graphs_key], function(obj) {
        files.push(Object.keys(obj)[0]);
    })
    print_multiple(files, graph_div, graph_name, vars_arr);
}

/**
 * Plot individual graphs
 */
function load_graph(graph_key) {
    // Hide home graphs and show general graphs
    $('#graph').show();
    $('.home_graph').hide();
    graph_file = report[graph_key];
    graph_name = graph_names[graph_key];
    print_chart(graph_file, graph_name, vars);
}

/**
 * Read the content and create the dropdown tabs
 */
function create_tabs() {
    var tabs = document.getElementById('tabs');
    var home_data = content.home;
    tabs.appendChild(create_element('home', 'Home', 'main'));

    $.each(home_data, function(key, value) {
        var tab_key = Object.keys(value)[0];
        dd_html = get_dropdown(content[tab_key]);

        // Special case if we have drop downs
        var elem = '';
        if(dd_html != '') {
            elem = create_dd_element(tab_key, value[tab_key], dd_html);
            elem.setAttribute('class', 'dropdown');
            elem.setAttribute('data-dropdown', 'dropdown');
        } else
            elem = create_element(tab_key, value[tab_key], 'main');

        tabs.appendChild(elem);
    });
    function get_dropdown(dd_data) {
        if(Object.size(dd_data) > 0) {
            var ul = document.createElement('ul');
            ul.setAttribute('class', 'dropdown-menu');
            $.each(dd_data, function(k, v) {
                var dd_key = Object.keys(v)[0];
                var e = create_element(dd_key, v[dd_key], 'sub');
                ul.appendChild(e);
            });
            return ul;
        } else
            return '';
    }

    function create_element(id, html, type) {
        var li = document.createElement('li');
        var a = document.createElement('a');
        a.href = 'javascript:void(0);';

        switch(type) {
            case 'main':
                a.setAttribute('onclick', 'load_tab("' + id + '")');
                break;
            case 'sub':
                a.setAttribute('onclick', 'load_graph("' + id + '")');
                break;
        }

        a.id = id;
        a.innerHTML = html;
        li.appendChild(a);
        return li;
    }

    function create_dd_element(id, html, append_data) {
        var li = document.createElement('li');
        var a = document.createElement('a');
        a.href = 'javascript:void(0);';
        a.id = id;
        a.setAttribute('class', 'dropdown-toggle');
        a.innerHTML = html;
        a.setAttribute('class', 'dropdown-toggle');
        li.appendChild(a);
        if( typeof (append_data) != "undefined" && append_data != '')
            li.appendChild(append_data);
        return li;
    }

}

/*------------------------------------------------------------------------------
Graph functions
-----------------------------------------------------------------------------*/

/**
 * Plots the graph
 */
function print_chart(file, name, vars_arr) {
    line_chart(get_transformed_data(file, vars_arr), 'graph', name);
}

/**
 * Plots a single graph from multiple data points
 */
function print_multiple(files, graph_div, name, vars_arr) {
    var data = new Object();
    $.each(files, function(k, graph_key) {
        var prefix = graph_names[graph_key];
        var file = report[graph_key];
        data[prefix] = get_transformed_data(file, vars_arr);
    });
    line_chart(index_merge(data), graph_div, name);
}

/**
 * Plots the graph using data as defined in the index file
 * TODO: This is deprecated. Delete or fix this
 */
function print_index(file, vars) {
    var files = read_index(file);
    var data = new Object();
    $.each(files, function(prefix, file) {
        data[prefix] = get_transformed_data(file, vars_arr);
    });
    line_chart(index_merge(data));
}

/**
 * Print chart in the container "graph"
 */
function line_chart(chart_data, graph_div, name) {
    var series_data = new Array();
    $.each(chart_data, function(key, value) {
        series_data.push({
            name : key,
            data : string_to_float_array(value)
        });
    });
    var length = get_max_length(series_data);

    var xaxis = new Array();
    for(var i = 1; i <= length; i++) {
        xaxis.push(i * time_interval);
    }

    var chart = new Highcharts.Chart({
        chart : {
            renderTo : graph_div,
            defaultSeriesType : 'spline',
            marginRight : 130,
            marginBottom : 30
        },
        title : {
            text : name,
            x : -20 //center
        },
        subtitle : {
            text : '',
            x : -20
        },
        xAxis : {
            title : {
                text : 'Time (Seconds)'
            },
            labels : {
                formatter : function() {
                    return xaxis[this.value];
                }
            }
        },
        yAxis : {
            title : {
                text : 'Count'
            },
            plotLines : [{
                value : 0,
                width : 1,
                color : '#808080'
            }]
        },
        tooltip : {
            formatter : function() {
                return '<b>' + this.series.name + '</b>' + this.y + '@' + this.x + 's';
            }
        },
        legend : {
            layout : 'vertical',
            align : 'right',
            verticalAlign : 'top',
            x : -10,
            y : 25,
            borderWidth : 1
        },
        series : series_data
    });
}

/*------------------------------------------------------------------------------
Conversion functions
-----------------------------------------------------------------------------*/

/**
 * Convert array from string elements to float elements
 */
function string_to_float_array(arr) {
    var ret = new Array();
    $.each(arr, function(key, value) {
        ret[key] = parseFloat(value);
    });
    return ret;
}

/**
 * Only return data in obj that has variables listed in vars
 */
function filter(obj, vars) {
    var ret = new Object();

    $.each(obj, function(key, value) {
        if(vars.indexOf(key) != -1)
            ret[key] = value;
    });
    return ret;
}

/**
 * Get the max length of the object
 * This is specialized for graph data and is not to be used elsewhere
 */
function get_max_length(data) {
    var max = 0;
    $.each(data, function(key, value) {
        length = data[key]['data'].length;
        if(length > max)
            max = length;
    });
    return max;
}

/**
 * Get data from CSV file and return in data required for the graph
 * @param file
 * @return
 */
function get_transformed_data(file, vars) {
    var csv = get_csv(file);
    chart_data = transform_array(csv);
    return filter(chart_data, vars);
}

/**
 * Transform the data of a 2D array into that suitable for graphing
 */
function transform_array(data) {
    var ret = new Object();
    var header = data[0];
    var length = data.length;

    $.each(header, function(index, value) {
        value = value.replace(/ /g, '');
        ret[value] = new Array();
        // Ignore the last value since its not of the same interval
        for(var i = 1; i < length - 1; i++) {
            val = data[i][index].replace(/ /g, '');
            ret[value].push(val);
        }
    });
    return ret;
}

/**
 * Read index file for key value pairs with prefix and file names
 */
function read_index(file) {
    var csv = get_csv(file);
    var ret = new Object();

    $.each(csv, function(key, value) {
        ret[csv[key][0]] = csv[key][1];
    })
    return ret;
}

/**
 * Merge multiple files into single chart data
 */
function index_merge(obj) {
    var ret = new Object();
    $.each(obj, function(prefix, result) {
        $.each(result, function(column, data) {
            ret[prefix + '::' + column] = data;
        });
    });
    return ret;
}

/*------------------------------------------------------------------------------
Util functions
-----------------------------------------------------------------------------*/

/**
 * Print data to console, if debugging is enabled
 */
function print(data) {
    if(debug)
        console.log(data);
}

Object.size = function(obj) {
    var size = 0, key;
    for(key in obj) {
        if(obj.hasOwnProperty(key))
            size++;
    }
    return size;
};
/**
 * Get file via ajax
 */
function get(file) {
    return data = $.ajax({
        url : file,
        type : "GET",
        async : false
    }).responseText;
}

/**
 * Read text data from the given file
 */
function get_csv(file) {
    return $.csv()(get(file));
}