/*jshint unused:true, eqnull:true, curly:true, bitwise:true */
/*jshint undef:true, latedef:true, trailing:true */
/*jshint browser:true */
/*global CodeMirror:true */

function sandebox() {
  "use strict";

  function xhr_handler(cm,updater,xhr_reply) {
    if (updater) {
      updater(cm,{});
    }
    document.getElementById("result").innerText = xhr_reply.srcElement.response;
  }

  function xhr_request(cm,updater) {
    var xhr = new XMLHttpRequest();
    var run = document.getElementById("run").value;
    var code = cm.getValue();
    xhr.open("POST", "code", true);
    xhr.onloadend = function(xhr_reply) { xhr_handler(cm,updater,xhr_reply); };
    xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xhr.send("code="+encodeURIComponent(code)+"&run="+encodeURIComponent(run));
  }

  var editor = CodeMirror.fromTextArea(
                 document.getElementById("code"),
                 {mode: "erlang",
                  theme: "erlang-dark",
                  lineNumbers: true,
                  matchBrackets: true,
                  extraKeys: {"Tab":  "indentAuto"},
                  lint: {async: true,
                         getAnnotations: xhr_request,
                         delay: 2000}
                 });

  var clik = function() { xhr_request(editor); };
  document.getElementById("eval_button").addEventListener("click",clik,false);

}
window.onload = sandebox;
