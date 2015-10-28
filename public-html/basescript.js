/*
@licstart

basescript.js - main javascript functions for Michal Grochmal's personal page

Copyright (C) 2013 Michal Grochmal

This JavaScript code is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License (GNU GPL) as published by the
Free Software Foundation, either version 3 of the License, or (at your option)
any later version.

This JavaScript code is distributed WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
the GNU GPL for more details.

The full text of the license can be found in the gpl3.txt file, on this server.
If you cannot find this file, see <http://www.gnu.org/licenses/>.

@licend
*/

// window.location.reload(true) to bypass cache.

function urlup(url, num) {
  var here = url.split('/');
  for (var i = 0; i < num; i++) here.pop();
  return here.join('/');
}

function showelem(self_id, id) {
  var elem = document.getElementById(id);
  if (null === elem)
    console.error( "showelem [called at " + String(self_id)
                 + "]: id " + String(id) + " do not exists in this document");
  else
    elem.style.visibility = "visible";
}

function hideelem(self_id, id) {
  elem = document.getElementById(id);
  if (null === elem)
    console.error( "hideelem [called at " + String(self_id)
                 + "]: id " + String(id) + " do not exists in this document");
  else
    elem.style.visibility = "hidden";
}

function append_submenu(pgbase, prnt, tbclass, idx, newobj) {
  da = document.createElement("div");
  da.className = tbclass[idx % 2];
  an = document.createElement("a");
  an.href = pgbase + newobj.href;
  if ("undefined" !== typeof newobj.img) {
    img = document.createElement("img");
    img.alt = newobj.img.alt;
    img.src = pgbase+newobj.img.src;
    img.className = newobj.img.class;
    an.appendChild(img);
  }
  an.appendChild(document.createTextNode(newobj.a));
  da.appendChild(an);
  prnt.appendChild(da);
}

function build_menus(pgbase, menu_res) {
  var req = new XMLHttpRequest();
  req.onreadystatechange = (function() {
    if (4 === req.readyState) {
      if (200 !== req.status) {
        console.error('build_menus: could not retrieve ' + String(menu_res));
        return null;
      }
      menu = JSON.parse(req.responseText);
      ids = Object.keys(menu.menu);
      ids.forEach(function(e,idx,arr) {
        var submenu = document.getElementById(e);
        if (null === submenu) {
          console.error("build_menus: element id " + e + " not present");
          return null;
        }
        var tbclass  = { "tbon":["tboff","tbon"] , "tboff":["tbon","tboff"] };
        var parclass = submenu.parentNode.parentNode.className;
        console.log('build_menus: for '+e+' (class '+parclass+')');
        menu.menu[e].forEach(function(e,idx,arr) {
          append_submenu(pgbase, submenu, tbclass[parclass], idx, e);
        });
      });
    }
  });
  req.open("GET", menu_res, true);
  req.setRequestHeader('Accept',        'application/json');
  req.setRequestHeader('Cache-Control', 'no-cache');
  req.send();
  return null;
}

