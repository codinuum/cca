
var exports = {};

function getPageHeight() {
  if ( window.innerHeight ) {
    return window.innerHeight;
  }
  else if ( document.documentElement && document.documentElement.clientHeight != 0 ) {
    return document.documentElement.clientHeight;
  }
  else if ( document.body ) {
    return document.body.clientHeight;
  }
  return 0;
}

function getParams() {
  var result = {};
  if(window.location.search.length > 1) {
    var query = window.location.search.substring(1);
    var params = query.split( '&' );

    for(var i = 0; i < params.length; i++) {
      var param = params[i].split('=');
      var name = decodeURIComponent(param[0]);
      var value = decodeURIComponent(param[1]);

      result[name] = value;
    }
  }
  return result;
}

var lang_tbl = { "text/x-fortran" : ['f08','F08','f03','F03','f95','F95','f90','F90','f','F','for','FOR'],
                 "text/x-csrc"    : ['c','h'],
                 "text/x-c++src"  : ['cpp','hpp','cc','hh','C','H'],
                 "text/x-java"    : ['java'],
                 "text/x-python"  : ['py'],
                 "text/x-verilog" : ['v'],
               };

var ext_tbl = {};

for (var l in lang_tbl) {
  var exts = lang_tbl[l];
  for (var i = 0; i < exts.length; i++)
    ext_tbl[exts[i]] = l;
}

function getLang(name) {
  var xs = name.split(".");
  var ext = xs[xs.length-1];//.toLowerCase();
  return ext_tbl[ext];
}
