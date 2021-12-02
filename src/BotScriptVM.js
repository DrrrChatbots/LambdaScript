
exports.newObject = () => ({})

unit = () => false
//unit = () => Object()

exports.evalBin = op => lval => rval => {
  lval = lval.valueOf();
  rval = rval.valueOf();
  if(op == "%")
    return (lval % rval);
  else if(op == "/")
    return (lval / rval);
  else if(op == '*')
    return (lval * rval);
  else if(op == '+')
    return (lval + rval);
  else if(op == '-')
    return (lval - rval);
  else if(op == '>')
    return (lval > rval);
  else if(op == '>=')
    return (lval >= rval);
  else if(op == '<')
    return (lval < rval);
  else if(op == '<=')
    return (lval <= rval);
  else if(op == '==')
    return (lval == rval);
  else if(op == '!=')
    return (lval != rval);
  else if(op == '===')
    return (lval === rval);
  else if(op == '!==')
    return (lval !== rval);
  else if(op == '&&')
    return (lval && rval);
  else if(op == '||')
    return (lval || rval);
  else if(op == 'in')
    return (lval in rval);
}

exports.evalUna = op => val => {
  val = val.valueOf();
  if(op == "!")
    return (!val);
  else if(op == '-')
    return (-val);
  else if(op == '_++' || op == '++_')
    return (val + 1);
  else if(op == '_--' || op == '--_')
    return (val - 1);
}

exports.evalApp = objm => obj => name => args => {

  //console.log("call => ", obj, name, args);
  args = args.map((x)=> x && x.valueOf ? x.valueOf() : x);

  val = undefined;
  try{
    if(!name){
      if(typeof(obj) == 'function')
        val = obj.apply(null, args);
      else console.log(`${objm} is not a function`)
    }
    else if(obj){
      if(obj[name]){
        if(typeof(obj[name]) == 'function')
          val = obj[name].apply(obj, args);
        else console.log(`${objm}.${name} is not a function`)
      }
      else console.log(`${name} of ${objm} is undefined`)
    }
    else val = undefined;
  }
  catch(err){
    console.log(String(err));
    return unit();
  }

  return val === undefined || val == null ? unit() : val;
}

invokFunction = namespace => syms => {
  var f = namespace;
  for(var key of syms){
    if(!f){
      console.log(`cannot find function ${syms}`)
      return undefined;
    }
    f = f[key];
  }
  return f;
}

exports.memberOf = obj => name => {
  val = obj[name];
  return val === undefined || val == null ? unit() : val;
}

exports.updMem = obj => name => val => () => {
  obj[name] = val;
}

exports.none = unit

exports.bool = pred => thn => els => {
  return pred.valueOf() ? thn : els;
}

//exports.timers = {}
//exports.setTimer = state => prd => act => () => {
//  exports.timers[state] = exports.timers[state] || [];
//  exports.timers[state].push(setInterval(act, prd));
//}

//exports.clearAllTimer = () => {
//  for(let s in exports.timers){
//    for(let id of exports.timers[s]){
//      clearInterval(id);
//    }
//  }
//  exports.timers = {};
//}

//exports.clearTimer = state => () => {
//  if(exports.timers[state])
//    for(id of exports.timers[state])
//      clearInterval(id);
//  exports.timers[state] = [];
//}

function padArray(array, length, fill){
  return length > array.length ?
    array.concat(Array(length - array.length).fill(fill)):
    array;
}

exports.meetEvent = machine => state => types => args => next => () => {
  machine.events[state] = machine.events[state] || [];

  [user_regex, cont_regex] = padArray(args, 2, "");

  machine.events[state].push([
    types, user_regex, cont_regex, next
  ])
  //console.log(`Event ${types} ${JSON.stringify(args)}`);
}

exports.dropEvent = events => state => () => {
  if(state) events[state] = [];
}

exports.dropTimer = timers => state => () => {
  if(timers[state])
    for(let id of timers[state])
      clearInterval(id);
  timers[state] = [];
}

exports.hangTimer = timers => state => prd => act => () => {
  timers[state] = timers[state] || [];
  timers[state].push(setInterval(act, prd));
}

exports.toNumber = Number
exports.toBoolean = Boolean

exports.toVaArgFunction = (fn) => {
  return function(...args){
    return fn([args].concat(args))();
  }
}

exports.stringify = JSON.stringify

exports["new"] = cons => args => {
  return new (cons.bind.apply(cons, [null].concat(args)))()
}

exports["delete"] = cons => attr => {
  if(attr) delete cons[attr];
  else delete cons;
}

exports.setMachine = machine => keys => values => {
  for(let i = 0; i < keys.length; i++)
    machine[keys[i]] = values[i];
  return machine;
}

// built-ins
stringify = obj => {
  str = JSON.stringify(obj)
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
