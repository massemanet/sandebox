/*jshint unused:true, eqnull:true, curly:true, bitwise:true */
/*jshint undef:true, latedef:true, trailing:true */
/*jshint browser:true */
/*global CodeMirror:true */

function sandebox() {
  "use strict";

  function erlang_linter(cm,updater) {
    req(cm);
    updater(cm,{});
  }

  var editor = CodeMirror.fromTextArea(
                 document.getElementById("code"),
                 {mode: "erlang",
                  theme: "erlang-dark",
                  lineNumbers: true,
                  matchBrackets: true,
                  extraKeys: {"Tab":  "indentAuto"},
                  lint: {async: true,
                         getAnnotations: erlang_linter,
                         delay: 2000}
                 });

  function replyhandler(oEvent) {
    document.getElementById("result").innerText = oEvent.srcElement.response;
  }

  function req(cm) {
    var xhr = new XMLHttpRequest();
    var run = document.getElementById("run").value;
    var code = cm.getValue();
    xhr.open("POST", "code", true);
    xhr.onloadend = replyhandler;
    xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xhr.send("code="+encodeURIComponent(code)+"&run="+encodeURIComponent(run));
  }

  var clik = function() { req(editor); };
  document.getElementById("eval_button").addEventListener("click",clik,false);

}
window.onload = sandebox;
