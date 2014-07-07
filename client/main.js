window.yasl = {}





yasl.pursuer = function () {
  
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
  
  
}
yasl.pursuer = yasl.pursuer()





yasl.semantic = function () {
  
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
  
}
yasl.semantic = yasl.semantic()





yasl.initializer = function () {
  
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
  
}
yasl.initializer = yasl.initializer()





yasl.explorer = function () {
  
  var Semantic = yasl.semantic
  
  return function (initial_state, datas) {
  
    var id = 0
    var path = []
    var tree = $("<div>")
      .addClass("tree")
      .tree({
        data:[nodify(initial_state)],
        autoOpen:true,
        dragAndDrop:false,
        onCreateLi: function (node, $li) {
          if (node.children.length === 0) {
            $li.find(".jqtree-element")
              .append($("<button>")
                .html("Step")
                .click(function () { step(1, node) }))
              .append($("<button>")
                .html("Step 10")
                .click(function () { step(10, node) }))
              .append($("<button>")
                .html("Run")
                .click(function () { step(0, node) }))
          }
          if (path.indexOf(node.id) !== -1) { $li.addClass("current") }
          if (node.state && node.state.success) { $li.addClass("success") }
          if (node.state && node.state.failure) { $li.addClass("failure") }
        }
      })
    var viewer = $("<div>")
      .addClass("viewer")
    var jqo = $("<div>")
      .addClass("explorer")
      .append($("<h2>").html("Explorer"))
      .append(tree)
      .append(viewer)
  
    function nodify (state) {
      var label
      if (state.success) {
        label = "Success: "+JSON.stringify(state.success)
      } else if (state.failure) {
        label = "Failure: "+JSON.stringify(state.failure)
      } else if (state.expression) {
        label = state.expression.substring(0,50)
      } else {
        label = "Unknown state"
      }
      return {
        state:state,
        label:label,
        id:++id,
        children:[]
      }
    }
  
    // Path //
    function set_current_id (id) {
      path.push(id)
      $(tree.tree("getNodeById", id).element).addClass("current")
    }
    function get_current_id () {
      return path[path.length-1]
    }
    set_current_id(1)
  
    function explore (state, callback) {
      var copy = JSON.parse(JSON.stringify(state))
      for (var i=0; i<datas.length; i++) { copy.heap[2][i] = datas[i] }
      Semantic(copy, callback)
    }
  
    function transit (parent, transition) {
      if (transition.next) {
        tree.tree("appendNode", nodify(transition.next), parent)
      } else if (transition.fork) {
        var fork = {
          fork:transition.fork,
          label:"Fork: ?"+transition.fork,
          promises:transition.heap[2],
          id:++id
        }
        tree.tree("appendNode", fork, parent)
        fork = tree.tree("getNodeById", fork.id)
        var consequent = nodify({
          expression:transition.consequent,
          environment:transition.environment,
          kontinuation:transition.kontinuation,
          heap:transition.heap
        })
        var alternative = nodify({
          expression:transition.alternative,
          environment:transition.environment,
          kontinuation:transition.kontinuation,
          heap:transition.heap        
        })
        tree.tree("appendNode", consequent, fork)
        tree.tree("appendNode", alternative, fork)
      } else {
        alert("Unknown transition: "+JSON.stringify(transition))
      }
    }
  
    tree.bind("tree.click", function (event) {
      if (event.node.state) {
        viewer.empty().append($(renderjson(event.node.state)))
      }
    })
  
    function step (counter, node) {
      function loop (state) {
        counter = counter - 1
        explore(state, function (transition) {
          if (transition.next && transition.next.expression && counter) {
            previous = transition.next
            loop(transition.next)
          } else {
            transit(node, transition)
          }
        })
      }
      loop(node.state)
    }
  
    // function step (node) {
    //   if (node.children.length === 0) {
    //     explore(node.state, function (transition) { transit(node, transition) })
    //   }
    // }
  
    jqo.$next = function () {
      var current = tree.tree("getNodeById", get_current_id())
      if (current.state) {
        if (datas.length < current.state.heap[2].length) { return current.state.heap[2][datas.length] }
        if (current.children.length === 1) { set_current_id(current.children[0].id); return jqo.$next() }
        if (current.children.length !== 0) { alert("Unknown node: "+JSON.stringify(current)) }
        return null
      } else if (current.fork) {
        if (datas.length < current.promises.length) { return current.promises[datas.length] }
        if (current.children.length !== 2) { alert("Unknown node: "+JSON.stringify(current)) }
        set_current_id(datas[current.fork] ? current.children[0].id : current.children[1].id)
        return jqo.$next()
      } else {
        alert("Unknown node: "+JSON.stringify(current))
      }
    }
  
    return jqo
  
  }
  
}
yasl.explorer = yasl.explorer()





yasl.main = function () {
  
  var Initializer = yasl.initializer
  var Pursuer = yasl.pursuer
  var Explorer = yasl.explorer
  
  $(function () {
  
    var evaluator = $("<div>")
  
    var initializer = Initializer()
      .on("initialize", function (evt, state) {
        var datas = []
        var pursuer = Pursuer(datas)
        var explorer = Explorer(state, datas)
        var button = $("<button>")
          .html("Catch up")
          .click(function () {
            var thunk = explorer.$next()
            if (thunk) {
              button.prop("disabled", true)
              pursuer.$catchup(thunk, function () {
                button.prop("disabled", false)
              })
            }
          })
        evaluator
          .empty()
          .append(explorer)
          .append(pursuer)
          .append(button)
      })
  
  
    $("body")
      .append($("<div>")
        .addClass("main")
        .append($("<h1>").html("Kusasa"))
        .append(initializer)
        .append(evaluator))
  
  })
  
}
yasl.main = yasl.main()
