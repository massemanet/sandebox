function load() {

  function tickhandler(oEvent) {
    document.getElementById("result").innerText = oEvent.srcElement.response;
  };

  function req() {
    var oReq = new XMLHttpRequest();
    run = document.getElementById("run").value;
    code = document.getElementById("code").value;
    oReq.open("POST", "code", true);
    oReq.onloadend = tickhandler;
    oReq.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    oReq.send("code="+encodeURIComponent(code)+"&run="+encodeURIComponent(run));
  };

  document.getElementById("eval_button").addEventListener("click",req,false);

}
window.onload = load;
