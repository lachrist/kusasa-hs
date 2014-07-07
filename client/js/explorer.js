
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
