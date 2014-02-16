function load() {

  function tickhandler(oEvent) {
    document.getElementById("result").innerHTML = oEvent.srcElement.response;
  };

  function req() {
    var oReq = new XMLHttpRequest();
    run = document.getElementById("run").innerHTML;
    code = document.getElementById("code").innerHTML;
    oReq.open("POST", "code", true);
    oReq.onloadend = tickhandler;
    oReq.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    oReq.send("code="+code+"&run="+run);
  };

  document.getElementById("eval_button").addEventListener("click",req,false);

}
window.onload = load;
