function load() {

  var editor = CodeMirror.fromTextArea(document.getElementById("code"), {
      lineNumbers: true,
      matchBrackets: true,
      extraKeys: {"Tab":  "indentAuto"},
      theme: "erlang-dark"
  });

  function tickhandler(oEvent) {
    document.getElementById("result").innerText = oEvent.srcElement.response;
  }

  function req() {
    var xhr = new XMLHttpRequest();
    var run = document.getElementById("run").value;
    var code = editor.getValue();
    xhr.open("POST", "code", true);
    xhr.onloadend = tickhandler;
    xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xhr.send("code="+encodeURIComponent(code)+"&run="+encodeURIComponent(run));
  }

  document.getElementById("eval_button").addEventListener("click",req,false);

}
window.onload = load;
