
function showThunk (thunk) {
  return thunk.function+"("+thunk.arguments.map(showTerm).join(", ")+")"
}

function showTerm (term) {
  if (term.Right) { return (typeof term.Right === "string") ? JSON.stringify(term.Right) : term.Right }
  return "?"+term.Left
}

return function (datas) {
  
  var ready = true

  var history = $("<ul>")
    .addClass("history")
  // var current = $("<div>")
  //   .addClass("current")
  var input = $("<input>")
    .prop("type", "text")
    .prop("disabled", true)
  var button = $("<button>")
    .html("Input")
    .prop("disabled", true)
  var output = $("<div>")
    .addClass("output")

  var jqo = $("<div>")
    .addClass("pursuer")
    .append($("<h2>").html("Pursuer"))
    .append($("<div>")
      .append($("<h3>").html("Promises"))
      .append(history))
      // .append($("<div>")
      //   .append($("<span>").html("Current promise:")
      //   .append(current))))
    .append($("<div>")
      .append($("<h3>").html("Input - Output"))
      .append(output)
      .append($("<div>")
        .append(input)
        .append(button)))

  jqo.$catchup = function (thunk, callback) {
    if (!ready) { return false }
    ready = false
    //current.html("?"+datas.length+" = "+showThunk(thunk)+"]")
    var args = thunk.arguments.map(function (term) { return (typeof term.Left !== "undefined") ? datas[term.Left] : term.Right })
    function end (data) {
      history.append($("<div>").html("?"+datas.length+" = "+JSON.stringify(data)+"   ["+showThunk(thunk)+"]"))
      //current.html("")
      datas.push(data)
      ready = true
      callback()
    }
    switch (thunk.function) {
      // Type
      case "?number?":  end(typeof args[0] === "number"); break
      case "?string?":  end(typeof args[0] === "string"); break
      case "?boolean?": end(args[0] === true || args(1) === false); break
      case "?null?":    end(args[0] === null); break
      // Equal
      case "?equal?": end(args[0] === args[1]); break
      // Boolean
      case "?and": end(Boolean(args[0] && args[1])); break
      case "?or":  end(Boolean(args[0] && args[1])); break
      case "?not": end(Boolean(!args[0])); break
      // Number
      case "?<":  end(args[0] <  args[1]); break
      case "?<=": end(args[0] <= args[1]); break
      case "?+":  end(args[0] +  args[1]); break
      case "?-":  end(args[0] +  args[1]); break
      case "?*":  end(args[0] *  args[1]); break
      case "?/":  end(args[0] /  args[1]); break
      // String
      case "?string->data":
        var data = null
        try { data = JSON.parse(args[0]) } catch (err) { }
        end(data)
        break
      case "?data->string": end(JSON.stringify(args[0])); break
      // Actions
      case "!read":
        input.prop("disabled", false)
        button.prop("disabled", false)
        button.one("click", function () {
          end(input.val())
          input.val("")
          input.prop("disabled", true)
          button.prop("disabled", true)
        })
        break
      case "!write":
        output.append($("<div>").html(+"> "+args[0]+"\n"))
        end(null)
        break
    }
    return true
  }

  jqo.$solve = function (promises) {
    promises = promises.slice()
    for (var i=0; i<datas.length; i++) {
      promises[i] = datas[i]
    }
    return promises
  }

  jqo.$next = function () { return datas.length }

  jqo.$fork = function (symbol) { return Boolean(datas[symbol]) }

  return jqo

}

