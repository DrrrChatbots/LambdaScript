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

exports.evalRef = val => idxs => {
  for(var key of idxs){
    if(!val) return Object(val);
    val = val[key];
  }
  return val['value0'] ? val : Object(val);
}

exports.evalFun = syms => args => {
  val = invokFunction(globalThis)(syms).apply(null, args);
  return val === undefined ? "unit" : val;
}

invokFunction = namespace => syms => {
  var f = namespace;
  for(var key of syms){
    if(!f)
    return () => console.log(`cannot find function ${syms}`)
    f = f[key];
  }
  return f;
}
