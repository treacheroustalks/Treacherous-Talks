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

/*------------------------------------------------------------------------------
 Global variables
 -----------------------------------------------------------------------------*/
var debug = true;
var random_divid_size = 6;
// Files to be loaded
var menu_file = 'menu.json';
var compare_file = 'compare.json';
var info_file = 'info.json';
// Loaded objects
var menu;
var compare;
var info;

/*------------------------------------------------------------------------------
Load functions
-----------------------------------------------------------------------------*/

// Initialize
function init() {
    menu = $.parseJSON(get(menu_file));
    compare = $.parseJSON(get(compare_file));
    info = $.parseJSON(get(info_file));
    create_tabs();

    load_hack_graph();
}

// Load the graph
function load_graph(graph_key) {
    // Clear current graph, if any
    $('#graphs').html('');

    // Check if the graph is a single graph or a compare graph
    // Single graphs
    if(info[graph_key] != undefined) {
        // Print all plots
        $.each(info[graph_key].plots, function(i, plot) {
            // Load the data from the file, if not already loaded
            var file = plot.file;
            graph_data_load(graph_key, file);
            plot.data = prep_plot_data(info[graph_key][file], plot.values, plot.plot_name);
            plot.div = setup_div();
            chart(plot);
        })
    // Multiple graphs
    } else if(compare[graph_key] != undefined) {
        $.each(compare[graph_key].plots, function(i, plot) {
            var total_graph_data = new Object();

            // Iterate through all the sources and load them
            $.each(compare[graph_key].sources, function(i, obj) {
                var graph_key = Object.keys(obj)[0];
                var file = plot.file;
                graph_data_load(graph_key, file);
                var graph_data = prep_plot_data(info[graph_key][file], plot.values, obj[graph_key]);
                $.extend(total_graph_data, graph_data);
            });
            plot.data = total_graph_data;
            plot.div = setup_div();
            chart(plot);
        });
    } else {
        print("Unknown graph");
    }
}

// Hacked graph for one use only!
// FIXME: Delete or fix this after use!
function load_hack_graph() {
    var graph_key = "scaling-compare-opt";

    $.each(compare[graph_key].plots, function(i, plot) {
        // Only the first graph
        if(i == 0) {
            var total_graph_data = new Object();

            // Iterate through all the sources and load them
            $.each(compare[graph_key].sources, function(i, obj) {
                var graph_key = Object.keys(obj)[0];
                var file = plot.file;
                plot.values = ["successful"];
                graph_data_load(graph_key, file);
                var graph_data = prep_plot_data(info[graph_key][file], plot.values, obj[graph_key]);
                $.extend(total_graph_data, graph_data);
            });
            total_graph_data = normalize(total_graph_data);

            // Create a copy of the object to avoid updating the original
            var plot_hack = jQuery.extend({},plot);

            // Plot normalized
            plot_hack.div = setup_div();
            plot_hack.data = total_graph_data;
            chart(plot_hack);

            // Plot summation
            plot_hack.xfunc = "summation";
            plot_hack.div = setup_div();
            plot_hack.data = total_graph_data;
            chart(plot_hack);

            // Plot averages
            var averages = get_avg(total_graph_data);
            plot_hack.xfunc = undefined;
            plot_hack.div = setup_div();
            plot_hack.data = {'scale': averages};
            plot_hack.interval = 1;
            chart(plot_hack);
        }
    });

    function get_avg(total_graph_data) {
        var ret = new Array();

        $.each(total_graph_data, function(key, val){
            ret.push(get_avg_array(val));
        });

        function get_avg_array(arr) {
            var total = 0;
            for(var i = 0; i < arr.length; i++)
                total = total + arr[i];

            return total/arr.length;
        }

        return ret;
    }

    function normalize(graph_data) {
        var ret = new Object();
        $.each(graph_data, function(key, val){
            ret[key] = norm_array(key, val, graph_data);
        });

        function norm_array(key, val, graph_data) {
            var ret = new Array();
            $.each(val, function(i, v){
                var denom = graph_data['1x:successful'][i];

                if(denom > 0)
                    ret.push(v/denom);
            });

            return ret;
        }

        return ret;
    }
}

// Load the graph data from the csv file into info
function graph_data_load(graph_key, file) {
    if(info[graph_key][file] == undefined) {
        var full_file = info[graph_key].location + '/' + file;
        info[graph_key][file] = get_graph_data(full_file);
    }
}

/*------------------------------------------------------------------------------
Graph functions
-----------------------------------------------------------------------------*/

// Print chart
function chart(plot) {
    // Convert plot data into graph series format
    var series_data = new Array();
    $.each(plot.data, function(key, value) {
        series_data.push({
            name : key,
            data : string_to_float_array(value)
        });
    });

    // Scale x-axis based on report interval
    var length = get_max_length(series_data);
    var xaxis = new Array();
    for(var i = 1; i <= length; i++) {
        xaxis.push(i * plot.interval);
    }

    // Run the series though function, if any
    if(plot.xfunc != undefined)
        switch(plot.xfunc) {
            case "summation":
                series_data = func_summation(series_data);
        }

    // Create the chart
    var chart = new Highcharts.Chart({
        chart : {
            renderTo : plot.div,
            defaultSeriesType : 'spline',
            marginRight : 130,
            marginBottom : 30
        },
        title : {
            text : plot.name,
            x : -20 //center
        },
        subtitle : {
            text : '',
            x : -20
        },
        xAxis : {
            title : {
                text : plot.xlabel
            },
            labels : {
                formatter : function() {
                    return xaxis[this.value];
                }
            }
        },
        yAxis : {
            title : {
                text : plot.ylabel
            },
            plotLines : [{
                value : 0,
                width : 1,
                color : '#808080'
            }]
        },
        tooltip : {
            formatter : function() {
                return this.series.name + '<b>' + this.y + '</b>' + '@' + '<b>' + this.x + '</b>' + 's';
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

    // Add info about the graph
    append_info(plot.div, plot.info);
}

// Summation function for graph
function func_summation(series) {
    var ret = new Array();
    $.each(series, function(i, obj) {
        ret.push({
            name : obj.name,
            data : sum_array(obj.data)
        });
    });

    function sum_array(arr) {
        var ret_array = new Array();
        var total = 0;
        $.each(arr, function(key, value) {
            total = total + value;
            ret_array[key] = total;
        });
        return ret_array;
    }

    return ret;
}

/*------------------------------------------------------------------------------
 Conversion functions
 -----------------------------------------------------------------------------*/

function get_graph_data(file) {
    var csv = get_csv(file);
    return transform_array(csv);
}

// Transform the data of a 2D array into that suitable for graphing
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

// Only return data in obj that has variables listed in vars
function filter(obj, vars) {
    var ret = new Object();

    $.each(obj, function(key, value) {
        if(vars.indexOf(key) != -1)
            ret[key] = value;
    });
    return ret;
}

function prep_plot_data(graph_data, vars, prefix) {
    var filtered_data = filter(graph_data, vars);
    var ret = new Object();
    $.each(filtered_data, function(name, value) {
        ret[prefix + ':' + name] = value;
    });
    return ret;
}

/*------------------------------------------------------------------------------
Page functions
-----------------------------------------------------------------------------*/

// Read the content and create the dropdown tabs
function create_tabs() {
    var tab_div = document.getElementById('tabs');

    $.each(menu, function(name, drop_array) {
        dd_html = get_dropdown(drop_array);

        var tab = create_tab(name, name, dd_html);
        tab.setAttribute('class', 'dropdown');
        tab.setAttribute('data-dropdown', 'dropdown');

        tab_div.appendChild(tab);
    });

    function get_dropdown(dd_data) {
        var ul = document.createElement('ul');
        ul.setAttribute('class', 'dropdown-menu');
        $.each(dd_data, function(i, obj) {
            key = Object.keys(obj)[0];
            var e = create_element(key, obj[key]);
            ul.appendChild(e);
        });
        return ul;
    }

    function create_element(id, html) {
        var li = document.createElement('li');
        var a = document.createElement('a');
        a.href = 'javascript:void(0);';
        a.setAttribute('onclick', 'load_graph("' + id + '")');
        a.id = id;
        a.innerHTML = html;
        li.appendChild(a);
        return li;
    }

    function create_tab(id, html, append_data) {
        var li = document.createElement('li');
        var a = document.createElement('a');
        a.href = 'javascript:void(0);';
        a.id = id;
        a.setAttribute('class', 'dropdown-toggle');
        a.innerHTML = html;
        li.appendChild(a);
        li.appendChild(append_data);
        return li;
    }

}

// Create a new div, appends it to graph and returns the id
function setup_div() {
    var div_id = randomString(random_divid_size);
    var div = document.createElement('div');
    div.id = div_id;
    div.setAttribute('class', 'graph');
    $('#graphs').append(div);
    return div_id;
}

function append_info(div_id, html) {
    var div = document.createElement('div');
    div.innerHTML = html;
    div.setAttribute('class', 'graph_info');
    $('#' + div_id).append(div);
}

/*------------------------------------------------------------------------------
Utility functions
-----------------------------------------------------------------------------*/

// Print data to console, if debugging is enabled
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
// Get file via ajax
function get(file) {
    return data = $.ajax({
        url : file,
        type : "GET",
        async : false
    }).responseText;
}

// Read text data from the given file
function get_csv(file) {
    return $.csv()(get(file));
}

// Returns random string of specified size
function randomString(len, charSet) {
    charSet = charSet || 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    var randomString = '';
    for(var i = 0; i < len; i++) {
        var randomPoz = Math.floor(Math.random() * charSet.length);
        randomString += charSet.substring(randomPoz, randomPoz + 1);
    }
    return randomString;
}

// Convert array from string elements to float elements
function string_to_float_array(arr) {
    var ret = new Array();
    $.each(arr, function(key, value) {
        ret[key] = parseFloat(value);
    });
    return ret;
}

// Get the max length of the object
// This is specialized for graph data and is not to be used elsewhere
function get_max_length(data) {
    var max = 0;
    $.each(data, function(key, value) {
        length = data[key]['data'].length;
        if(length > max)
            max = length;
    });
    return max;
}
