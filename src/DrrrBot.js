stringify = obj => {
  try{
    str = JSON.stringify(obj);
  }
  catch(err){
    return '[Circular Object]';
  }
  if(obj === undefined)
    str = "undefined";
  else if(typeof obj == 'function')
    str = 'function' + (obj.name ? ' ' + obj.name : '');
  else if(str === undefined && obj.toString)
    str = obj.toString();
  else if(str === '{}' && obj.constructor
    && obj.constructor.name != 'Object')
    str = "[Object " + obj.constructor.name + "]"
  return str;
}

botlang_builtins = {
  'title': function(msg){
    console.log(`title ${JSON.stringify(msg)}`);
  },
  'descr': function(msg){
    console.log(`descr ${JSON.stringify(msg)}`);
  },
  'print': function(...args){
    console.log.apply(null,
      args.map((e)=> stringify(e.valueOf ? e.valueOf() : e)));
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

for(let key in botlang_builtins){
  globalThis[key] = botlang_builtins[key];
}

exports.cur = ""
exports.events = {}

function padArray(array, length, fill){
  return length > array.length ?
    array.concat(Array(length - array.length).fill(fill)):
    array;
}

// ignore
exports.clearAllEvent = () => {
  exports.cur = "";
  exports.events = {};
}

// done
exports.setcur = state => () => {
  if(exports.cur) exports.events[exports.cur] = [];
  exports.cur = state;
}

// exports.listen = state => types => args => next => () => {
exports.listen = types => args => next => () => {
  let state = exports.cur;
  exports.events[state] = exports.events[state] || [];

  [user_regex, cont_regex] = padArray(args, 2, "");

  exports.events[state].push([
    types, user_regex, cont_regex, next
  ])

  //console.log(`Event ${types} ${JSON.stringify(args)}`);
}
