/**
@licstart

remakes.js - javascript functions for IWT 1st coursework.

Copyright (C) 2013 Michal Grochmal

This JavaScript code is free software: you can redistribute it and/or modify it
under the terms of the Creative Commons Attribution-ShareAlike 3.0 Unported
(CC BY-SA 3.0) as published by creative commons.  The license limitations are
given by the licensing obligation when submitting on Moodle.

This JavaScript code is distributed WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
the GNU GPL for more details.

@licend

-----------
Declaration
-----------

I confirm that this coursework submission is entirely my own work, except where
explicitly stated otherwise.

Much of the code is based on examples on the Mozilla Developer Network,
which can be found under http://developer.mozilla.org .  Where such
code is present it is sated as "<this code> is based on MDN examples".

Some code was reused from the lecture slides of the IWT module:
http://www.dcs.bbk.ac.uk/~ptw/teaching/IWT.html , these are annotated
as "<this code> is reused from the lecture slides"

When other sources were used these are stated next to the relevant code.

---------------------
How this script works
---------------------

The remakes global variable stores the configuration of this script (i.e. where
the resources needed for execution can be found) and the current state of the
page.  This state includes these resources already loaded into memory.

On page load we load into memory the XML data file (from it's main location or
from the fallback location) and the XSLT stylesheet.  We then do a best effort
to fix inconsistencies in the loaded XML, if the fixes cannot be performed we
throw in default consistent values instead of the read data.  At the end of
this process the search button is enabled for the user.  If the page cannot
access the necessary data the search button is never enabled.

The corrected XML data is kept in browser memory whilst the user stays on the
page, therefore no data need to be loaded or corrected between searches.

The user types in the filters, selects options and clicks the search button. On
the click the filters are evaluated and if they're incorrect the user is warned
and asked to read the fine manual and examples.  If the filters are incorrect
no search is performed.  Apart from the manual text, the filter syntax shall be
easily understandable from the examples and from the filters present in the
text boxes when the page loads.

If the filters typed are correct they are transformed into JavaScript functions
and then combined with the values of the AND/OR buttons to form one big filter.
Then the values of the radio buttons for sorting and ordering are retrieved.
During the first search these values are used to modify the XSLT stylesheet,
which is applied to the XML data to produce an unfiltered table.  During
subsequent searches the radio button values are compared to the values from the
previous search and the XSLT processing is triggered only if the sorting or
ordering needs to change.  Otherwise, the current unfiltered table have the
data in the correct order an there is no need to create a new table from the
XML data once again.

Finally the unfiltered table is filtered using the filter composed from the
individual filters combined with the AND/OR buttons.  The matched results are
left visible, whilst the records that do not match are collapsed using the CSS
visibility property.

If no results match the "No movies to display, sorry..." text is displayed in
the table body.  Otherwise the result table is displayed.

-----
Notes
-----

This is the first time I wrote a big piece of code in JavaScript, therefore I
wanted to learn the most of it whilst doing this work.  I played with
JavaScript as a functional language using higher-order functions partial
function application and function composition extensively, as well as array
(a.k.a. list) processing instead of loops.
*/
var remakes = {};
remakes.set_next_val  = { 'OR':'AND' , 'AND':'OR' };
remakes.data_path     = 'http://www.dcs.bbk.ac.uk/~ptw/teaching/IWT/coursework/remakes.xml';
remakes.data_fallback = 'remakes.xml';
remakes.stylesheet    = 'remakes.xsl';
remakes.data          = null;  // the corrected XML data
remakes.style         = null;  // the XSLT stylesheet
remakes.sort          = null;  // last sorting choice of the user
remakes.display       = null;  // last ordering choice of the user

// dirty (but very useful) hack, based on MDN examples
HTMLCollection.prototype.forEach = Array.prototype.forEach;
NodeList.prototype.forEach       = Array.prototype.forEach;

// change buttons between AND and OR
function union_inter(but) { but.value = remakes.set_next_val[but.value]; }

/* CSS visibility is based on MDN examples.
 * Chrome have some issues when collapsing too many table rows at the same
 * time: on Chrome some extra padding pixels get inserted between visible
 * table rows when too many rows are collapsed between them.  This causes
 * no issue in program logic or display of correct results, but
 * sometimes it may look ugly. */
function showtr(elem) {
  elem.style.top        = null;
  elem.style.left       = null;
  elem.style.padding    = '1px';
  elem.style.position   = 'static';
  elem.style.visibility = 'visible';
}
function hidetr(elem) {
  elem.style.top        = '0px';
  elem.style.left       = '0px';
  elem.style.padding    = 0;
  elem.style.position   = 'absolute';
  elem.style.visibility = 'collapse';
}

/* function composition and partial application are taken (and modified)
 * from the book Eloquent JavaScript by Marijn Haverbeke */
function as_array(arg_arr, start) {
  var result = [];
  for (var i = (start || 0); i < arg_arr.length; i++) result.push(arg_arr[i]);
  return result;
}
function compose(f1, f2) {
  return function() { return f1(f2.apply(null, arguments)); };
}
function partial(f) {
  var args = as_array(arguments, 1);
  return function() {return f.apply(null, args.concat(as_array(arguments)));};
}

// deal with hiding and showing the "no results" part of the page
function no_results(id, show) {
  div = document.getElementById(id);
  div.innerHTML = '';
  if (show)
    div.appendChild(document.createTextNode('No movies to display, sorry...'));
}

function append_new_table(table, id) {
  div = document.getElementById(id);
  div.innerHTML = '';
  div.appendChild(table);
}

// XSLT processing reused from the coursework document and lecture slides
function generate_table(xmldoc, style, sortby, display) {
  var types  = { 'rtitle':'text' , 'ryear':'number'
               , 'stitle':'text' , 'syear':'number'
               , 'fraction':'number'
               },
      nsres  = style.createNSResolver(
                 null == style.ownerDocument ?
                 style.documentElement : style.ownerDocument.documentElement),
      sorter = style.evaluate('//xsl:sort', style, nsres,
                              XPathResult.FIRST_ORDERED_NODE_TYPE, null),
      value  = sorter.singleNodeValue;
      value.setAttribute('select',    sortby);
      value.setAttribute('order',     display);
      value.setAttribute('data-type', types[sortby]);
  var proc = new XSLTProcessor();
  proc.importStylesheet(style);
  return proc.transformToFragment(xmldoc, document);
}

// applies the constructed filter to all rows, hiding the unneeded ones
function filter_rows(filter, rows) {
  var results = 0;
  rows.forEach(function(e) {
    if (filter(e.children)) {
      showtr(e);
      results += 1;
    } else {
      hidetr(e);
    }
  });
  return results;
}

/* Collect information from the radio buttons and decide if a new table needs
 * to be constructed from the XML.  If so generate a new table and filter it,
 * otherwise filter the already existing table.  In either case keep track of
 * the number of results so we can tell the user when there were no results. */
function process_table(filter) {
  var sort = "rtitle", display = "ascending";
  document.getElementsByName('sort').forEach(function(e) {
    if (e.checked) sort = e.value;
  });
  document.getElementsByName('display').forEach(function(e) {
    if (e.checked) display = e.value;
  });
  no_results('no_results', false);  // we might have results clean it
  if (sort !== remakes.sort || display !== remakes.display) {
    remakes.sort    = sort;               // sorting changed so we need to
    remakes.display = display;            // generate a new table
    var table   = generate_table(remakes.data, remakes.style,
                                 remakes.sort, remakes.display),
        rows    = table.firstChild.children[1].children,
        results = filter_rows(filter, rows);
        append_new_table(table, 'results');
  } else {  // we already have a properly sorted table, reuse it
    var table   = document.getElementById('results').firstElementChild,
        rows    = table.children[1].children,
        results = filter_rows(filter, rows);
  }
  if (0 >= results) no_results('no_results', true);  // no results tell it
}

// gives a function from a comparison operator
function get_operator(value) {
  var operators = {};
  operators['<'] = function(a,b) { return b <  a; };
  operators['>'] = function(a,b) { return b >  a; };
  operators['='] = function(a,b) { return b == a; };
  return operators[value[0]] || operators['='];
}

/* generates a JavaScript function from a filter typed
 * by the user in the year or fraction fields */
function numeric_filter(valid, filter) {
  if ("" === filter) return function(x) { return true; };
  var op   = get_operator(filter);
  var tail = filter.replace(/^[<>\=]\s*/, '');
  if (!valid(tail)) {
    alert(filter + " is not a valid search argument, please RTFM above");
    return null;
  }
  return partial(op, parseFloat(tail));
}

// regex escape based on MDN examples
function escape_regex(str) {
  return str.replace(/([.*+?^=!:${}()|\[\]\/\\])/g, "\\$1");
}

// generates a JavaScript function from the title field and both check boxes
function title_filter(apprx, samet, filter) {
  if ("" === filter) return function(x) { return true; };
  var same = function(t1,t2) { return true; };
  if (samet)
    same = (function(t1,t2) { return t1.toLowerCase() === t2.toLowerCase(); });
  if (apprx) {
    return function(title, org_title) {
      return (same(title, org_title)
              && Boolean(title.match(new RegExp(escape_regex(filter), "i"))));
    };
  } else {
    return function(title, org_title) {
      return (same(title, org_title)
              && title.toLowerCase() === filter.toLowerCase());
    };
  }
}

function valid_year(year) { return Boolean(year.match(/^\d+$/));    }
function valid_frac(frac) { return Boolean(frac.match(/^0\.\d+$/)); }

/**
 * -----------------------------
 * Filter construction algorithm
 * -----------------------------
 *
 * Defining the order in which the filters shall be applied is not trivial with
 * two set operation buttons and three optional fields (2^2*3^2 possibilities).
 * The algorithm of how the composite filter is constructed is described below:
 *
 * Each field that is left blank is considered to be an always true condition
 * and the button next to it must always have the AND operation (even if the
 * user explicitly changes the button to OR, for ORing with an always true case
 * on one side will always return all records).  When all filter fields are
 * present there's no issue with the set conditions and we can safely use
 * whatever the user set the buttons to.
 *
 * When two or all three fields are empty there's no issue on deciding which
 * set operations to use.  In both cases we shall AND all filters: in the case
 * of two empty filters it will apply the only filter to all records; and in
 * the case of three empty filters it will return all records, as intended by
 * the user.
 *
 * But this becomes harder when only one filter is left empty.  If the empty
 * filter is on the far right or far left we need to AND that position ignoring
 * the value on the given button, because:
 *   [true AND cond1 AND/OR cond2] is equivalent to [cond1 AND/OR cond2]
 * and
 *   [cond1 AND/OR cond2 AND true] is equivalent to [cond1 AND/OR cond2]
 *
 * Finally, when the middle filter is empty we need to decide between which
 * button to use for the condition, we will use the following mnemonic
 * transformation:
 *   [cond1 AND true AND cond2] => [cond1 AND cond2]
 *   [cond1 AND true OR  cond2] => [cond1 AND cond2]
 *   [cond1 OR  true AND cond2] => [cond1 AND cond2]
 *   [cond1 OR  true OR  cond2] => [cond1 OR  cond2]
 *
 * This algorithm is implemented in the code below. */
function corr_conds(set_left, set_right, fields) {
  var empty = 0;
  fields.forEach(function(e) { if ('' == e) empty++; });
  if (0 == empty) return set_left+'_'+set_right;
  if (2 == empty) return 'AND_AND';
  if (3 == empty) return 'AND_AND';
  var fld = fields.indexOf('');     // we have 1 empty field, which?
  if (0 >  fld)   return 'AND_AND'  // just in case
  if (0 == fld)   return 'AND_'+set_right;
  if (2 == fld)   return set_left+'_AND';
  if ('OR' == set_left && 'OR' == set_right) return 'AND_OR';
  return 'AND_AND';
}

// auxiliary function to reduce repetition in the code below
function t_vl(elem) { return elem.firstChild.nodeValue; }

var search_order = {};                               // define order of filters
search_order.AND_AND = function(fy, ff, ft, tr) {    //  c1 AND c2  AND c3
  return fy(t_vl(tr[1])) && ff(t_vl(tr[4])) && ft(t_vl(tr[0]),t_vl(tr[2]));
};
search_order.AND_OR  = function(fy, ff, ft, tr) {    // (c1 AND c2) OR  c3
  return (fy(t_vl(tr[1])) && ff(t_vl(tr[4]))) || ft(t_vl(tr[0]),t_vl(tr[2]));
};
search_order.OR_AND  = function(fy, ff, ft, tr) {    //  c1 OR (c2  AND c3)
  return fy(t_vl(tr[1])) || (ff(t_vl(tr[4])) && ft(t_vl(tr[0]),t_vl(tr[2])));
};
search_order.OR_OR   = function(fy, ff, ft, tr) {    //  c1 OR  c2  OR  c3
  return fy(t_vl(tr[1])) || ff(t_vl(tr[4])) || ft(t_vl(tr[0]),t_vl(tr[2]));
};

function do_search() {
  var year_text  = document.getElementById('year_text'    ).value.trim(),
      fraction   = document.getElementById('faithful_text').value.trim(),
      title_text = document.getElementById('title_text'   ).value.trim(),
      apprx      = document.getElementById('match_check'  ).checked,
      samet      = document.getElementById('stitle_check' ).checked,
      set_left   = document.getElementById('set_left'     ).value,
      set_right  = document.getElementById('set_right'    ).value,
      set_conds  = corr_conds(set_left, set_right,
                              [ year_text, fraction, title_text ]),
      filter_ord = search_order[set_conds],
      filters    = {};
  if (!(filters.year  = numeric_filter(valid_year, year_text ))) return false;
  if (!(filters.frac  = numeric_filter(valid_frac, fraction  ))) return false;
  if (!(filters.title = title_filter(apprx, samet, title_text))) return false;
  var final_f = partial(filter_ord, filters.year, filters.frac, filters.title);
  process_table(final_f);
  return true;
}

/**
 * --------------
 * Initialisation
 * --------------
 *
 * All procedures below are needed only once, just after the HTML body finishes
 * loading.  Later, we operate on the XML document in memory, this proves to be
 * faster that loading it remotely on each search and safer because we need to
 * perform error checking and recovery just once.
 *
 * As all fixes to the data in the XML happen during the initialisation we
 * can consider the data in the XML as consistent in the rest of the script
 * (i.e. during every search). */

// check for empty text nodes
function is_empty(value) {
  if (null == value) return true;  // catch both null and undefined
  if (!value.trim()) return true;
  return false;
}

// fixes a remake if it misses a certain data node or if the data node is empty
function fix_node(prnt, dflt, name) {
  var elem = prnt.getElementsByTagName(name)[0];
  if (null == elem) {
    elem = document.createElement(name);
    prnt.appendChild(elem);
  }
  if (null == elem.firstChild) elem.appendChild(document.createTextNode(dflt));
  if (is_empty(elem.firstChild.nodeValue)) elem.firstChild.nodeValue = dflt;
  return elem;
}

// ensures that all unknown titles are set to the same value: "unknown"
function fix_title(elem) {
  var text = elem.firstChild.nodeValue.trim();
  if ('' == text || 'unknown'.match(new RegExp('^'+escape_regex(text), 'i')))
    elem.firstChild.nodeValue = 'unknown';
  else if (elem.firstChild.nodeValue !== text)
    elem.firstChild.nodeValue = text;
  return elem;
}

// best effort procedure to fix the year, adds a default otherwise
function fix_year(elem) {
  var text = elem.firstChild.nodeValue;
  if (valid_year(text)) return elem;
  if (text.match(/\d\d\d\d/)) text = text.replace(/^.*(\d\d\d\d).*$/, '$1');
  if (valid_year(text))
    elem.firstChild.nodeValue = text;
  else
    elem.firstChild.nodeValue = '1972';
  return elem;
}

// best effort procedure to fix the fraction, adds a default otherwise
function fix_frac(elem) {
  var text = elem.firstChild.nodeValue;
  if (valid_frac(text)) return elem;
  if ('.' === text[0]) text = '0'+text;
  if (text.match(/^0\.\d+/)) text = text.replace(/^(0\.\d+).*/, '$1');
  if (valid_frac(text))
    elem.firstChild.nodeValue = text;
  else
    elem.firstChild.nodeValue = '0.5';
  return elem;
}

/* a play with function composition and partial application: for each remake
 * element fixing functions (that are created on the fly) are applied to its
 * relevant data nodes */
function fix_xml(dom) {
  dom.getElementsByTagName('remake').forEach(function(e) {
    (compose(fix_title, partial(fix_node, e, 'unknown')))('rtitle');
    (compose(fix_year,  partial(fix_node, e, '1973'   )))('ryear');
    (compose(fix_title, partial(fix_node, e, 'unknown')))('stitle');
    (compose(fix_year,  partial(fix_node, e, '1963'   )))('syear');
    (compose(fix_frac,  partial(fix_node, e, '0.5'    )))('fraction');
  });
  return dom;
}

/* The size of the XSLT stylesheet is << than the size of the XML
 * document to transform.  If we get the stylesheet asynchronously
 * whilst the main thread is busy fetching the XML we can be rather
 * sure that the stylesheet will already be in memory when the search
 * button becomes enabled. */
function get_stylesheet(path) {
  var req = new XMLHttpRequest();
  req.onreadystatechange = (function() {
    if (4 == req.readyState) {
      if (200 != req.status) {
        console.error("Something horrible happened, there's no XSLT!");
        return null
      }
      remakes.style = req.responseXML;
    }
  });
  req.open("GET", path, true);
  req.setRequestHeader('Accept', 'text/xml');
  req.send();
}

/* Try to load the XML into memory directly from Peter's page, if we fail
 * (most likely due to same origin policy if this page is deployed somewhere
 * else than www.dcs.bbk.ac.uk) try to load a local copy. */
function load_xml(enable, path_xml, fall_back) {
  var req = new XMLHttpRequest();
  function send(path) {
    req.open("GET", path, false);
    req.setRequestHeader('Accept',        'text/xml');
    req.setRequestHeader('Cache-Control', 'no-cache');
    req.send();
    if (200 !== req.status) {
      console.error('load_xml: could not retrieve xml in ' + path);
      throw 'returned status ' + String(req.status);
    }
    remakes.data = fix_xml(req.responseXML);
    document.getElementById(enable).disabled = false;
  }
  try { send(path_xml);
  } catch (e) {
    console.error( 'load_xml: on path '+String(path_xml)+'('
                 + String(e)+') '+'try loading '+String(fall_back)+' instead');
    try { send(fall_back);
    } catch (e) {
      console.error('load_xml: no xml loaded, bail ('+e+')');
    }
  }
}

function dependencies_ok() {
  try {
    new XMLHttpRequest();  // This will print a properly formatted alert
    new XSLTProcessor();   // message depending on which component fails.
  } catch (e) {            // A hackerish solution, but easily extensible!
    alert(String(e).replace(/^[^:]*:\s*/, '')+' in your browser, bailing');
    return false;
  }
  return true;
}

// do the entire initialisation, it shall be easily extensible
function load_data() {
  if (!dependencies_ok()) {
    console.log('load_data: dependencies not present, cannot search');
    return null;
  }    // to add a new initialiser add a zero argument function into the array
  var enable = [ partial(get_stylesheet, remakes.stylesheet)
               , partial(load_xml, 'search_but',
                         remakes.data_path, remakes.data_fallback)
               ];
  enable.forEach(function(e) { e() });
}

