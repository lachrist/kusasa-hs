
/////////////
// Helpers //
/////////////

function path (name) {
  var comps = name.split(".")
  comps[0] = root
  return comps.join("/")+".js"
}

function populate (names, i, yasl) {
  if (i === names.length) { return yasl }
  var cur = yasl
  var dirs = names[i].split(".")
  for (var j=1; j<dirs.length-1; j++) {
    if (!cur[dirs[j]]) { cur[dirs[j]] = {} }
    cur = cur[dirs[j]]
  }
  return populate(names, i+1, yasl)
}

function indent (str) {
  return str.split("\n").map(function (line) { return "  "+line }).join("\n")
}

///////////////
// Variables //
///////////////

var fs = require("fs")

// node yasl.js <main> <root>
var main = process.argv[2]
var root = process.argv[3]

var names = []
var contents = {}
var pendings = {}

/////////////
// Explore //
/////////////

function explore (name, parents, k) {
  if (parents.indexOf(name) !== -1) {
    process.stderr.write("Cycle detected at "+parents.join(">>")+"\n")
    process.exit(1)
  }
  parents.push(name)
  if (Object.keys(contents).indexOf(name) !== -1) {
    (names.indexOf(name) !== -1) ? k() : pendings[name].push(k)
  } else {
    contents[name] = null
    pendings[name] = []
    fs.readFile(path(name), {encoding:"utf8"}, function (err, str) {
      if (err) {
        switch (err.errno) {
          case 34: process.stderr.write("The file "+path(name)+" from "+parents.join(">>")+" does not exists\n"); break
          case 3: process.stderr.write("No read permission on file "+parents.join(">>")+"\n"); break
          default: process.stderr.write("Unexpected error "+err+" at "+parents.join(">>")+"\n") 
        }
        process.exit(1)
      } else {
        contents[name] = str
        var childs = str.match(/yasl(\.[a-zA-Z_$][0-9a-zA-Z_$]*)+/g)
        if (!childs) { childs = [] }
        var rdv = childs.length+1
        function done () {
          rdv = rdv-1
          if (rdv === 0) {
            names.push(name)
            k()
            pendings[name].forEach(function (k) { k() })
            delete pendings[name]
          }
        }
        done()
        childs.forEach(function (child) { explore(child, parents.slice(), done) })
      }
    })
  }
}

//////////
// Main //
//////////

explore(main, [], function () {
  var yasl = populate(names, 0, {})
  process.stdout.write("window.yasl = "+JSON.stringify(yasl)+"\n")
  names.forEach(function (name) {
    process.stdout.write("\n\n\n\n\n")
    process.stdout.write(name+" = function () {\n"+indent(contents[name])+"\n}\n")
    process.stdout.write(name+" = "+name+"()\n")
  })
  process.exit(0)
})
