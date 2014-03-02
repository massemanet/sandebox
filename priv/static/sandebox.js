/*jshint unused:true, eqnull:true, curly:true, bitwise:true */
/*jshint undef:true, latedef:true, trailing:true */
/*jshint browser:true */
/*global CodeMirror:true */

function sandebox() {
  "use strict";

  function xhr_handler(cm,updater,xhr_reply) {
    var response = JSON.parse(xhr_reply.srcElement.response);
    switch (response.status) {
      case "ok":
        if (updater) {
          updater(cm,[]);
        }
        document.getElementById("result").innerText = response.result;
        break;
      case "error":
        if (updater) {
          var lints = [];
          var is = response.items;
          for ( var i=0; i < is.length; i++) {
            lints.push({from: CodeMirror.Pos(is[i].line-1,0),
                        to:   CodeMirror.Pos(is[i].line-1,0),
                        severity: is[i].severity,
                        message:  is[i].description});
          }
          updater(cm,lints);
          document.getElementById("result").innerText = "";
        }
        break;
      case "crash": {}
        break;
      default: {}
    }
  }

  function xhr_request(cm,updater) {
    var xhr = new XMLHttpRequest();
    var run = document.getElementById("run").value;
    var code = cm.getValue();
    xhr.open("POST","code",true);
    xhr.onloadend = function(xhr_reply) { xhr_handler(cm,updater,xhr_reply); };
    xhr.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    xhr.send("code="+encodeURIComponent(code)+"&run="+encodeURIComponent(run));
  }

  var editor = CodeMirror.fromTextArea(
                 document.getElementById("code"),
                 {mode: "erlang",
                  theme: "erlang-dark",
                  lineNumbers: true,
                  matchBrackets: true,
                  extraKeys: {"Tab":  "indentAuto"},
                  gutters: ["CodeMirror-lint-markers"],
                  lint: {async: true,
                         getAnnotations: xhr_request,
                         delay: 2000}
                 });

  var clik = function() { xhr_request(editor); };
  document.getElementById("eval_button").addEventListener("click",clik,false);

}
window.onload = sandebox;
