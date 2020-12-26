exports.evalBin = op => lval => rval => {
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
  if(op == "!")
    return Object(!val);
  else if(op == '-')
    return Object(-val);
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
