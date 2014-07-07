
var Semantic = yasl.semantic

var names = [
  // Primitives
  "success", "failure", "cons", "car", "cdr", "set-car!", "set-cdr!", "data?", "closure?", "action?", "function?",
  // Functions
    // Type
    "?number?", "?string?", "?boolean?", "?null?",
    // Equal
    "?equal?",
    // Boolean
    "?and", "?or", "?not",
    // Number
    "?<", "?<=", "?+", "?-", "?*", "?/",
    // String
    "?string->data", "?data->string",
  // Actions
  "!write", "!read"
]
function initial (program) {
  var global = {"(parent)": 0}
  names.forEach(function (name) { global[name] = {control:name} })
  return {
    expression:program,
    environment:1,
    kontinuation:null,
    heap:[[null, global], [], []]
  }
}

function execute (state, done, callback) {
  Semantic(state, function (transition) {
    if (!transition.next) {
      alert("Wrong transition in prelude: "+JSON.stringify(transition))
    } else if (transition.next.success) {
      alert("Success sate in prelude: "+JSON.stringify(transition.next.success))
    } else if (transition.next.failure) {
      alert("Failure sate in prelude: "+JSON.stringify(transition.next.failure))
    } else if (transition.next.heap[2].length > 0) {
      alert("Promises introduced in prelude: "+JSON.stringify(transition.next.heap[2]))
    } else if (done) {
      callback(transition.next)
    } else {
      done = transition.next.expression.indexOf("\"<code>\"") === 0
      execute(transition.next, done, callback)
    }
  })
}

function load_prelude (callback) {
  var settings = {
    type:"GET",
    dataType:"text",
    success:callback,
    error:function (arg1, arg2) { alert(JSON.stringify(arg1)+"\n\n"+JSON.stringify(arg2)) }
  }
  $.ajax("http://127.0.0.1:8000/scm/prelude.scm", settings)
}




return function () {

  var prelude = $("<textarea>")
    .prop("cols", 48)
    .prop("rows", 15)
    .prop("disabled", true)
  var code = $("<textarea>")
    .prop("cols", 48)
    .prop("rows", 15)
  var button = $("<button>")
    .html("Submit")
    .prop("disabled", true)
    .click(function () {
      button.prop("disabled", true)
      prelude.prop("disabled", true)
      code.prop("disabled", true)
      var state = initial("(begin "+prelude.val()+" \"<code>\" "+code.val()+")")
      execute (state, false, function (state) {
        jqo.trigger("initialize", state)
        button.prop("disabled", false)
        prelude.prop("disabled", false)
        code.prop("disabled", false)
      })
    })

  load_prelude(function (def) {
    prelude.val(def).prop("disabled", false)
    button.prop("disabled", false)
  })

  var jqo = $("<div>")
    .addClass("editor")
    .append($("<h2>").html("Editor"))
    .append($("<div>")
      .append($("<h3>").html("Prelude"))
      .append(prelude))
    .append($("<div>")
      .append($("<h3>").html("Code"))
      .append(code))
    .append(button)

  return jqo

}
