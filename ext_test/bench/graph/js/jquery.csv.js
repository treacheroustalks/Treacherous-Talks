/* Usage:
 *  jQuery.csv()(csvtext)               returns an array of arrays representing the CSV text.
 *  jQuery.csv("\t")(tsvtext)           uses Tab as a delimiter (comma is the default)
 *  jQuery.csv("\t", "'")(tsvtext)      uses single quotes as the quote character (double quotes is the default)
 *  jQuery.csv("\t", "'\"")(tsvtext)    uses single & double quotes as the quote character
 *  Note: Use jQuery.csv(delim, 0) to speed up loading, if there are no quotes. Use with care: there might be quotes
 *
 *  jQuery.csv2json()(csvtext)          returns an array of hashes, whose keys are from the header row.
 *                                      Otherwise, syntax identical to jQuery.csv()
 */

(function($) {

// split() doesn't work properly on IE. "a,,b".split(",") returns ["a", "b"] and not ["a", "", "b"]
// On IE, you need to fix String.prototype.split first. See http://blog.stevenlevithan.com/archives/cross-browser-split
if ('a,,b'.split(',').length < 3) {
    var nativeSplit = nativeSplit || String.prototype.split;
    String.prototype.split = function (s /* separator */, limit) {
        // If separator is not a regex, use the native split method
        if (!(s instanceof RegExp)) {
                return nativeSplit.apply(this, arguments);
        }

        /* Behavior for limit: If it's...
         - Undefined: No limit
         - NaN or zero: Return an empty array
         - A positive number: Use limit after dropping any decimal
         - A negative number: No limit
         - Other: Type-convert, then use the above rules */
        if (limit === undefined || +limit < 0) {
            limit = false;
        } else {
            limit = Math.floor(+limit);
            if (!limit) {
                return [];
            }
        }

        var flags = (s.global ? "g" : "") + (s.ignoreCase ? "i" : "") + (s.multiline ? "m" : ""),
            s2 = new RegExp("^" + s.source + "$", flags),
            output = [],
            lastLastIndex = 0,
            i = 0,
            match;

        if (!s.global) {
            s = new RegExp(s.source, "g" + flags);
        }

        while ((!limit || i++ <= limit) && (match = s.exec(this))) {
            var zeroLengthMatch = !match[0].length;

            // Fix IE's infinite-loop-resistant but incorrect lastIndex
            if (zeroLengthMatch && s.lastIndex > match.index) {
                s.lastIndex = match.index; // The same as s.lastIndex--
            }

            if (s.lastIndex > lastLastIndex) {
                // Fix browsers whose exec methods don't consistently return undefined for non-participating capturing groups
                if (match.length > 1) {
                    match[0].replace(s2, function () {
                        for (var j = 1; j < arguments.length - 2; j++) {
                            if (arguments[j] === undefined) { match[j] = undefined; }
                        }
                    });
                }

                output = output.concat(this.slice(lastLastIndex, match.index), (match.index === this.length ? [] : match.slice(1)));
                lastLastIndex = s.lastIndex;
            }

            if (zeroLengthMatch) {
                s.lastIndex++;
            }
        }

        return (lastLastIndex === this.length) ?
            (s.test("") ? output : output.concat("")) :
            (limit      ? output : output.concat(this.slice(lastLastIndex)));
    };
}



// Returns a function that splits a line into fields using a simple delimiter
// delimre is the regular expression for the delimiter, e.g. /,/
function simple_splitline(delimre) {
    return function(v) { return v.split(delimre); };
}

// Returns a function that splits a line into fields using a delimiter, and accounts for quotes.
// For example, quoted_splitline(/,/, /"/, /""/) will break the following line:
//      a,"b,c",d,"e,""f"",g"
// into
//      ['a', 'b,c', 'd', 'e,"f",g']
function quoted_splitline(delim, delimre, quotere, doublequotere) {
    return function(v) {
        var arr = v.split(delimre);
        for (var out=[], q, i=0, l=arr.length; i<l; i++) {
            // If the value is within quotes, then from the point where the quote begins ...
            if (q = arr[i].match(quotere)) {
                q = q[0];
                // ... to the point where the quote ends
                for (var j=i; j<l; j++) {
                    if (arr[j].charAt(arr[j].length-1) == q) { break; }
                }

                // Join the pieces into a single piece
                var s = arr.slice(i,j+1).join(delim);

                // Double quoting is the escape sequence for quotes. ("" instead of ")
                s = s.replace(doublequotere[q], q);

                // Use this joined piece instead of the individual pieces
                out.push(s.substr(1,s.length-2));
                i = j;
            }
            else {
                out.push(arr[i]);
            }
        }
        return out;
    };
}

function parse_param(delim, quote, lined) {
    var undef = 'undefined';
    delim = typeof delim == undef ? ','    : delim;         // delimiter is comma by default
    quote = typeof quote == undef ? '"'    : quote;         // quotes mark is "double quotes" by default
    lined = typeof lined == undef ? '\r\n' : lined;         // line delimiter is \r or \n or both
    var quotes  = quote ? quote.split('') : [],             // get each character in the quote
        delimre = new RegExp( '[' + delim + ']' ),
        quotere = new RegExp('^[' + quote + ']' );
    for (var i=0, doublequotere={}, q; q=quotes[i]; i++) {  // and make a dictionary of doublequote possibilities
        doublequotere[q] = new RegExp(q + q, 'g');
    }
    return [
        new RegExp( '[' + lined + ']*$' ),                                  // trailing line delimiter
        new RegExp( '[' + lined + '][' + lined + ']*'),                     // line delimiter,
        quote ? quoted_splitline(delim, delimre, quotere, doublequotere) :  // splitline function
                simple_splitline(delimre)
    ];
}

jQuery.extend({
    csv: function(delim, quote, lined) {
        var param = parse_param(delim, quote, lined),
            trailing  = param[0],
            linedre   = param[1],
            splitline = param[2];

        // The main function. Split into lines, and then call splitline repeatedly.
        return function(text) {
            var lines = text.replace(trailing, '').split(linedre);
            for (var i=0, l=lines.length; i<l; i++) {
                lines[i] = splitline(lines[i]);
            }
            return lines;
        };
    },

    csv2json: function(delim, quote, lined) {
        var param = parse_param(delim, quote, lined),
            trailing  = param[0],
            linedre   = param[1],
            splitline = param[2];

        // The main function. Split into lines, and then call splitline repeatedly.
        return function(text) {
            var lines = text.replace(trailing, '').split(linedre),  // split into lines
                header = splitline(lines[0]),                       // and get the first row (header)
                nfields = header.length,
                out = [];                                           // put the results into this array
            for (var i=1, l=lines.length; i<l; i++) {
                var line = splitline(lines[i]);                     // split each subsequent row
                for (var j=0, result={}; j<nfields; j++) {          // and make it a hash of fields, using the header
                    result[header[j]] = line[j];
                }
                out.push(result);                                   // add the hash
            }
            return out;
        };
    }
});

})(jQuery);
