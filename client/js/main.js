
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
