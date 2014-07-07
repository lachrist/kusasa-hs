
return function (state, callback) {
  var settings = {
    type:"POST",
    contentType:"application/json; charset=UTF-8",
    dataType:"json",
    data:JSON.stringify(state),
    success:callback,
    error:function (arg1, arg2) { alert(JSON.stringify(arg1)+"\n"+JSON.stringify(arg2)) }
  }
  $.ajax("http://127.0.0.1:8000", settings)
}
