exports.evalBin = op => lval => rval => {
  lval = lval.valueOf();
  rval = rval.valueOf();
  if(op == "%")
    return Object(lval % rval);
  else if(op == "/")
    return Object(lval / rval);
  else if(op == '*')
    return Object(lval * rval);
  else if(op == '+')
    return Object(lval + rval);
  else if(op == '-')
    return Object(lval - rval);
  else if(op == '>')
    return Object(lval > rval);
  else if(op == '>=')
    return Object(lval >= rval);
  else if(op == '<')
    return Object(lval < rval);
  else if(op == '<=')
    return Object(lval <= rval);
  else if(op == '==')
    return Object(lval == rval);
  else if(op == '!=')
    return Object(lval != rval);
  else if(op == '===')
    return Object(lval === rval);
  else if(op == '!==')
    return Object(lval !== rval);
  else if(op == '&&')
    return Object(lval && rval);
  else if(op == '||')
    return Object(lval || rval);
}

exports.evalUna = op => val => {
  val = val.valueOf();
  if(op == "!")
    return Object(!val);
  else if(op == '-')
    return Object(-val);
  else if(op == '_++' || op == '++_')
    return Object(val + 1);
  else if(op == '_--' || op == '--_')
    return Object(val - 1);
}

exports.evalFun = obj => name => args => {

  //console.log("call => ", obj, name, args);

  if(!name && typeof(obj) == 'function'){
    val = obj.apply(null, args);
  }
  else if(obj && obj[name]){
    val = obj[name].apply(obj, args);
  }
  else val = undefined;

  //console.log("val = ", val);

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

exports.print = obj => () => {
  console.log("this", obj, typeof(obj));
}

exports.bool = pred => thn => els => {
  return pred.valueOf() ? thn : els;
}
