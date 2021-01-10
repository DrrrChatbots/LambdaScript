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
      else console.log(objm, "is not a function")
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
    return {};
  }

  return val === undefined || val == null ? {} : val;
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
  return val === undefined || val == null ? {} : val;
}

exports.updMem = obj => name => val => () => {
  obj[name] = val;
}

exports.none = () => Object()

exports.bool = pred => thn => els => {
  return pred.valueOf() ? thn : els;
}

exports.timers = {}
exports.setTimer = state => prd => act => () => {
  exports.timers[state] = exports.timers[state] || [];
  exports.timers[state].push(setInterval(act, prd));
}

exports.clearAllTimer = () => {
  for(s in exports.timers){
    for(id of exports.timers[s]){
      clearInterval(id);
    }
  }
  exports.timers = {};
}

exports.clearTimer = state => () => {
  if(exports.timers[state])
    for(id of exports.timers[state])
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
