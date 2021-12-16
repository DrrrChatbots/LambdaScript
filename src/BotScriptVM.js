
let unit = () => false
//unit = () => Object()

function valueOf(v){
  if(v === undefined
  || v === null)
    return false.valueOf();
  if(v.valueOf)
    return v.valueOf();
  else
    return v;
}

exports.evalBin = op => lval => rval => {
  lval = valueOf(lval);
  rval = valueOf(rval);
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
  val = valueOf(val);
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
  args = args.map((x)=> x && x.valueOf ? x.valueOf() : (x === undefined || x === null ? false : x));

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

exports.timers = {}
exports.setTimer = state => prd => act => () => {
  exports.timers[state] = exports.timers[state] || [];
  exports.timers[state].push(setInterval(act, prd));
}

exports.clearAllTimer = () => {
  for(let s in exports.timers){
    for(let id of exports.timers[s]){
      clearInterval(id);
    }
  }
  exports.timers = {};
}

exports.clearTimer = state => () => {
  if(exports.timers[state])
    for(let id of exports.timers[state])
      clearInterval(id);
  exports.timers[state] = [];
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
