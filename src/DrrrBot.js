botlang_builtins = {
  'title': function(msg){
    console.log(`title ${JSON.stringify(msg)}`);
  },
  'descr': function(msg){
    console.log(`descr ${JSON.stringify(msg)}`);
  },
  'print': function(...args){
    console.log.apply(null,
      args.map((e)=>e.valueOf ? e.valueOf() : e));
    //console.log(`print ${JSON.stringify(msg)}`);
  },
  'order': function(keyword, p1, p2){
    console.log(`order ${JSON.stringify(keyword)}`);
  },
  'new': function (func) {
    var res = {};
    if (func.prototype !== null) {
      res.__proto__ = func.prototype;
    }
    var ret = func.apply(res,
      Array.prototype.slice.call(arguments, 1));
    if ((typeof ret === "object"
      || typeof ret === "function")
      && ret !== null) {
      return ret;
    }
    return res;
  }
}

for(key in botlang_builtins){
  globalThis[key] = botlang_builtins[key];
}

exports.cur = ""
exports.events = {}

function padArray(array, length, fill){
  return length > array.length ?
    array.concat(Array(length - array.length).fill(fill)):
    array;
}

exports.clearAllEvent = () => {
  exports.cur = "";
  exports.events = {};
}

exports.unlisten = state => () => {
  exports.cur = state;
  exports.events[state] = [];
}

exports.listen = state => types => args => next => () => {
  exports.events[state] = exports.events[state] || [];

  [user_regex, cont_regex] = padArray(args);

  exports.events[state].push([
    types, user_regex, cont_regex, next
  ])

  console.log(`Event ${types} ${JSON.stringify(args)}`);
}
